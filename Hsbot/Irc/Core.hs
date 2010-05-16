module Hsbot.Irc.Core
    ( startIrcbot
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time
import Network
import Prelude hiding (catch)
import System.IO

import Hsbot.Irc.Command
import Hsbot.Irc.Config
import Hsbot.Irc.Message
import Hsbot.Irc.Plugin
import Hsbot.Irc.Server
import Hsbot.Irc.Types
import Hsbot.Message (BotMsg)

-- | IrcBot's main entry point
startIrcbot :: IrcConfig -> Chan BotMsg -> Chan BotMsg -> IO ()
startIrcbot config masterChan myChan = do
    startTime <- getCurrentTime
    putStrLn "[IrcBot] Opening communication channel... "
    chan <- newChan :: IO (Chan IrcBotMsg)
    putStrLn $ concat ["[IrcBot] Connecting to ", ircConfigAddress config, "... "]
    handle <- connectTo (ircConfigAddress config) (ircConfigPort config)
    hSetBuffering handle NoBuffering
    myOwnThreadId <- myThreadId
    putStrLn "[IrcBot] Spawning reader threads..."
    readerThreadId <- forkIO $ ircBotReader handle chan myOwnThreadId
    masterReaderThreadId <- forkIO $ ircBotMasterReader masterChan chan
    putStrLn "[IrcBot] Initializing server connection..."
    let ircServerState = IrcServerState { ircServerId            = ircConfigAddress config
                                        , ircServerChannels      = []
                                        , ircServerNickname      = ircConfigNickname config
                                        , ircServerCommandPrefix = ircConfigCommandPrefix config
                                        , ircServerChan          = chan }
        ircBotState = IrcBotState { ircBotStartTime            = startTime
                                  , ircBotPlugins              = M.empty
                                  , ircBotCommands             = M.empty
                                  , ircBotChan                 = chan
                                  , ircBotMasterChan           = masterChan
                                  , ircBotMyChan               = myChan
                                  , ircBotServerState          = ircServerState
                                  , ircBotHandle               = handle
                                  , ircBotConfig               = config
                                  , ircBotReaderThreadId       = readerThreadId
                                  , ircBotMasterReaderThreadId = masterReaderThreadId }
    ircBotState' <- execStateT (initBotServerConnection config) ircBotState
    putStrLn "[IrcBot] Entering main loop... "
    _ <- ircBotLoop ircBotState' `catch` (\(_ :: IOException) -> return ())
    return ()

--resumeIrcBot
--resumeIrcBot

-- | Runs the IrcBot's reader loop
ircBotReader :: Handle -> Chan IrcBotMsg -> ThreadId -> IO ()
ircBotReader handle chan fatherThreadId = forever $ do
    str <- (hGetLine handle) `catch` handleIOException
    let msg = parseIrcMsg str
    case msg of
        Right msg' -> writeChan chan (InIrcMsg msg')
        _          -> return ()
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

-- | Reads the Master's chan
ircBotMasterReader :: Chan BotMsg -> Chan IrcBotMsg -> IO ()
ircBotMasterReader masterChan _ = forever $ do
    _ <- readChan masterChan
    return ()
    -- TODO : handle botMsg

-- | Initialize the bot's server connection
initBotServerConnection :: IrcConfig -> IrcBot ()
initBotServerConnection config = do
    ircBot <- get
    let ircServerState = ircBotServerState ircBot
    ircServerState' <- execStateT (initServerConnection config) ircServerState
    put $ ircBot { ircBotServerState = ircServerState' }

-- | IrcBot's loop that can catch ircbot's states' updates
ircBotLoop :: IrcBotState -> IO ()
ircBotLoop ircBotState = do
    putStrLn "[IrcBot] Spawning plugins..."
    ircBotState' <- execStateT spawnIrcPlugins ircBotState
    -- Todo : throw new ircbotstate to hsbot
    putStrLn "[IrcBot] Entering Core loop... "
    _ <- (execStateT ircBotCore ircBotState') -- `catch` (\(_ :: NewBotStateException) -> return ircBotState')
    return ()
    -- TODO : loop!

-- | Run the IrcBot's main loop
ircBotCore :: IrcBot ()
ircBotCore = forever $ do
    ircBot <- get
    let chan = ircBotChan ircBot
    msg  <- liftIO $ readChan chan
    case msg of
        InIrcMsg inIrcMsg   -> dispatchMessage $ InIrcMsg inIrcMsg
        OutIrcMsg outIrcMsg -> sendThisMessage outIrcMsg
        IntIrcCmd intIrcCmd -> do
            processInternalCommand $ IntIrcCmd intIrcCmd
  where
    sendThisMessage :: IrcMsg -> IrcBot ()
    sendThisMessage outputMsg = do
        let str = serializeIrcMsg outputMsg
        handle <- gets ircBotHandle
        liftIO $ hPutStr handle (str ++ "\r\n")

-- | Dispatches an input message
dispatchMessage :: IrcBotMsg -> IrcBot ()
dispatchMessage (InIrcMsg inIrcMsg) = do
    config  <- gets ircBotConfig
    plugins <- gets ircBotPlugins
    cmds    <- gets ircBotCommands
    if (isPluginCommand config)
      then
        let key         = tail . head $ words getMsgContent
            pluginNames = fromMaybe [] $ M.lookup key cmds
            plugins'    = fromMaybe [] $ mapM (flip M.lookup plugins) pluginNames
        in mapM_ (sendRunCommand (tail getMsgContent) . fst) plugins'
      else
        mapM_ (sendToPlugin (InIrcMsg inIrcMsg) . fst) (M.elems plugins)
  where
    isPluginCommand :: IrcConfig -> Bool
    isPluginCommand config =
        and [ ircMsgCommand inIrcMsg == "PRIVMSG"
        , (head getMsgContent) == ircConfigCommandPrefix config ]
    sendRunCommand :: String -> IrcPluginState -> IrcBot ()
    sendRunCommand cmd plugin =  sendToPlugin (IntIrcCmd $ IrcCmd "RUN" "CORE" (ircPluginName plugin) cmd inIrcMsg) plugin
    getMsgContent :: String
    getMsgContent = unwords . tail $ ircMsgParameters inIrcMsg
dispatchMessage _ = return ()

