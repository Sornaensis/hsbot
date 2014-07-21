{-# LANGUAGE OverloadedStrings #-}
module Hsbot.Core
    ( initHsbot
    , runHsbot
    , terminateHsbot
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import Crypto.Random.AESCtr 
import qualified Data.Map as M
import Network
import qualified Network.IRC as IRC
import Network.BSD (getHostName)
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger
import Text.ParserCombinators.Parsec

import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.Char8 as S


initHsbot :: Config -> IO BotEnv
initHsbot config = do
    chan <- newChan :: IO (Chan Message)
    botState <- newEmptyMVar
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    let hostname = configAddress config
        port = configPort config
    infoM "Hsbot.Core" $ "Connecting to " ++ hostname -- ++ ":" ++ port
    connhdl <- connectTo hostname port
    hSetBuffering connhdl NoBuffering
    hSetEncoding connhdl utf8
    (tls, tlsCtx) <- if sslOn $ configTLS config
        then (do
            infoM "Hsbot.Core" "Initializing TLS communication"
            tlsenv <- initTLSEnv config
            randomGen <- makeSystem
            sCtx <- contextNew connhdl tlsenv randomGen 
            handshake sCtx
            return (Just tlsenv, Just sCtx))
        else return (Nothing, Nothing)
    return BotEnv { envBotState    = botState
                  , envHandle      = connhdl
                  , envChan        = chan
                  , envQuitMv      = quitMv
                  , envThreadIdsMv = threadIdsMv
                  , envConfig      = config
                  , envTLS         = tls
                  , envTLSCtx      = tlsCtx }

strEncode :: IRC.Message -> String
strEncode = S.unpack . IRC.encode

runHsbot :: [String] -> Env IO BotStatus
runHsbot die_msgs = do
    botNotInitialized <- asks envBotState >>= liftIO . isEmptyMVar
    when botNotInitialized runFirstSteps
    trueRunHsbot
  where
    -- | Initialize the dialog with the IRC server
    runFirstSteps :: Env IO ()
    runFirstSteps = do
        env <- ask
        -- First we say hello
        hostname <- liftIO getHostName
        let connhdl  = envHandle env
            tlsCtx   = envTLSCtx env
            config   = envConfig env
            nickname = head $ configNicknames config
            channels = configChannels config
            cmdPrfx  = configCmdPrefix config 
        case configPassword config of
            Just pass -> liftIO . sendStr env connhdl tlsCtx . strEncode $ IRC.Message Nothing (S.pack "PASS") [(S.pack pass)]
            Nothing -> return ()
        liftIO . sendStr env connhdl tlsCtx . strEncode $ IRC.nick (S.pack nickname)
        liftIO . sendStr env connhdl tlsCtx . strEncode $ IRC.user (S.pack nickname) (S.pack hostname) (S.pack "*") $ S.pack (configRealname config)
        -- Finally we set the new bot state
        asks envBotState >>= liftIO . flip putMVar BotState { botPlugins  = M.empty
                                                            , botPrefix   = cmdPrfx 
                                                            , botAccess   = configAccess config
                                                            , botHooks    = []
                                                            , botChannels = channels
                                                            , botNickname = nickname }
    -- | Run the bot itself
    trueRunHsbot :: Env IO BotStatus
    trueRunHsbot = do
        env <- ask
        -- Next we spawn the reader thread
        liftIO $ debugM "Hsbot.Core" "Spawning reader thread"
        let connhdl  = envHandle env
            tlsCtx   = envTLSCtx env
            config   = envConfig env
            channels = configChannels config
        chan <- asks envChan
        (liftIO . forkIO $ botReader env connhdl tlsCtx chan) >>= addThreadIdToQuitMVar
        -- Then we spawn all plugins
        asks envConfig >>= mapM_ loadPlugin . configPlugins
        -- Finally we spawn the main bot loop
        (liftIO . forkIO $ runReaderT botLoop env) >>= addThreadIdToQuitMVar
        liftIO $ threadDelay 1000000
        -- Then we join channels
        mapM_ (\channel -> liftIO . sendStr env connhdl tlsCtx . strEncode . IRC.joinChan $ S.pack channel) channels
        -- We advertise any death message we should
        mapM_ (\msg -> mapM_ (\channel -> liftIO . sendStr env connhdl tlsCtx $ S.unpack . IRC.encode $ IRC.Message Nothing (S.pack "PRIVMSG") [S.pack channel, S.pack msg]) channels) die_msgs
        -- We wait for the quit signal
        code <- asks envQuitMv >>= liftIO . takeMVar
        -- and we clean things up
        asks envThreadIdsMv >>= liftIO . readMVar >>= liftIO . mapM_ killThread
        -- TODO : kill plugin threads
        return code

botReader :: BotEnv -> Handle -> Maybe Context -> Chan Message -> IO ()
botReader env handle mctx chan = do
    ioException <- botTrueReader "" `catch` return
    runReaderT (setGlobalQuitMVar $ BotRestart (show ioException, Just "botReader died")) env
  where
    botTrueReader :: String -> IO IOException
    botTrueReader buff = do
        str <- readThis handle mctx
        case parse messages [] (buff ++ str) of
            Right (msgs, trash) -> do
                mapM_ handleMessage msgs
                botTrueReader trash
            Left err -> do
                errorM "Hsbot.Reader" $ "Reader decode error (" ++ show err ++ ") on " ++ str
                botTrueReader ""
    messages = do
        msgs <- option [] $ many1 message
        trash <- option "" $ many1 anyChar
        return (msgs, trash)
    message = do
        mess <- many1 $ noneOf "\r\n"
        end <- string "\r\n" <|> string "\r" <|> string "\n"
        return $ mess ++ end
    handleMessage :: String -> IO ()
    handleMessage str =
        case IRC.decode (S.pack str) of
            Just msg -> do
                debugM "Hsbot.Reader" $ "<-- " ++ show msg
                writeChan chan $ IncomingMsg msg
            Nothing -> return ()
    readThis :: Handle -> Maybe Context -> IO String
    readThis _ (Just ctx) = fmap S.unpack (recvData ctx)
    readThis h Nothing = hGetLine h >>= \s -> return $ s ++ "\n"

botLoop :: Env IO ()
botLoop = forever $ do
    chan <- asks envChan
    msg  <- liftIO $ readChan chan
    hooks <- asks envBotState >>= liftIO . flip withMVar (return . botHooks)
    mapM_ (liftIO . flip writeChan msg) hooks
    case msg of
        IncomingMsg _ -> return () -- TODO parse for core commands
        OutgoingMsg outMsg -> do
            env <- ask
            let connhdl  = envHandle env
                tlsCtx   = envTLSCtx env
            liftIO $ debugM "Hsbot.Loop" $ "--> " ++ show outMsg
            liftIO . sendStr env connhdl tlsCtx $ S.unpack (IRC.encode outMsg)

terminateHsbot :: Env IO ()
terminateHsbot = do
    liftIO $ infoM "Hsbot.Core" "Closing connection"
    asks envHandle >>= liftIO . hClose

