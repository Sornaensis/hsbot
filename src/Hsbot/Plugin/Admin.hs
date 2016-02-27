module Hsbot.Plugin.Admin
    ( admin
    , theAdmin
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Network.IRC as IRC

import Hsbot.Message
import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U

-- | The Admin plugin identity
admin :: PluginId
admin = PluginId
    { pluginName = "admin"
    , pluginEp   = theAdmin defAdmin
    , pluginCmds = Just (theCmds defAdmin)  }

data AdminConfig = AdminConfig 
                   { exitMsg :: String
                   , killMsg :: String
                   , partMsg :: String }

defAdmin :: AdminConfig
defAdmin = AdminConfig
           { exitMsg = "Exiting..."
           , killMsg = "https://www.youtube.com/watch?v=vaR7Uxau9OY"
           , partMsg = "Whoops!" }

theCmds :: AdminConfig -> PluginCmds
theCmds config = [
            PluginCmd
            { command = "exit"
            , cmdIsPrefix = False
            , cmdFunc = exitBot $ exitMsg config
            , cmdPerm = Just Admin
            , cmdHelp = "Exit the bot entirely."
            }
          , PluginCmd
            { command = "quit"
            , cmdIsPrefix = False
            , cmdFunc = exitBot $ exitMsg config
            , cmdPerm = Just Admin
            , cmdHelp = "Exit the bot entirely."
            }
          , PluginCmd
            { command = "die"
            , cmdIsPrefix = False
            , cmdFunc = exitBot $ killMsg config
            , cmdPerm = Just Admin
            , cmdHelp = "Exit the bot entirely."
            }
          , PluginCmd
            { command = "killyourself"
            , cmdIsPrefix = False
            , cmdFunc = exitBot $ killMsg config
            , cmdPerm = Just Admin
            , cmdHelp = "Exit the bot entirely."
            }
          , PluginCmd
            { command = "join"
            , cmdIsPrefix = False
            , cmdFunc = joinChan
            , cmdPerm = Just Admin
            , cmdHelp = "Force the bot to join a new channel"
            }
          , PluginCmd
            { command = "part"
            , cmdIsPrefix = False
            , cmdFunc = partChan $ partMsg config
            , cmdPerm = Just Admin
            , cmdHelp = "Force the bot to part a channel"
            }
          , PluginCmd
            { command = "reload"
            , cmdIsPrefix = False
            , cmdFunc = reload
            , cmdPerm = Just Admin
            , cmdHelp = "Reload specified plugin"
            }
          , PluginCmd
            { command = "unload"
            , cmdIsPrefix = False
            , cmdFunc = unloadPlugin
            , cmdPerm = Just Admin
            , cmdHelp = "Unload specified plugin"
            }
        ]

unloadPlugin :: IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
unloadPlugin msg [] = return Nothing
unloadPlugin msg (arg:_) 
    | arg /= "admin" =
        lift (killPlugin arg) >>
        return Nothing
    | otherwise = return Nothing
        where
        killPlugin p = 
          asks envBotState >>= \state ->
          liftIO (readMVar (state)) >>= \ps -> 
              case  M.lookup p $ botPlugins ps of
              Just (_, th) -> liftIO (modifyMVar_ state (\s -> 
                                                                            return $ s {botPlugins = M.delete p (botPlugins ps)})) 
                                                  >> liftIO (killThread th)
              Nothing      -> return ()

reload :: IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
reload msg [] = return Nothing
reload msg ("bot":_) = do lift . setGlobalQuitMVar $ BotRestart (getSender msg ++ " request", Nothing)
                          return Nothing
reload msg (arg:_) 
    | arg /= "admin" =
        lift (killPlugin arg) >>
        return Nothing
    | otherwise = return Nothing
        where
        killPlugin p = 
          asks envBotState >>= \state ->
          liftIO (readMVar (state)) >>= \ps -> 
              case  M.lookup p $ botPlugins ps of
              Just ((PluginEnv tid _ _ ), th) -> liftIO (modifyMVar_ state (\s -> 
                                                                            return $ s {botPlugins = M.delete p (botPlugins ps)})) 
                                                  >> liftIO (killThread th) >> loadPlugin tid
              Nothing      -> return ()

exitBot :: String -> IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
exitBot exitmsg _   _ = do writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "QUIT") [U.fromString exitmsg] 
                           lift $ setGlobalQuitMVar BotExit
                           return Nothing

joinChan :: IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
joinChan _   []       = return Nothing
joinChan _   (chan:_) = do writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "JOIN") [U.fromString chan]
                           return Nothing

partChan :: String -> IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
partChan _       _   []       = return Nothing
partChan partmsg _   (chan:_) = do writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "PART") [U.fromString chan, U.fromString partmsg]
                                   return Nothing
-- | An IRC plugin for manage hsbot
theAdmin :: AdminConfig -> Plugin (Env IO) ()
theAdmin config = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" = 
            do
                cmdArgs <- lift $ getCommand msg
                void $ mapM_ (parseCommand msg cmdArgs) (theCmds config)
        | IRC.msg_command msg == U.fromString "NICK" =
            do
            botNick <- lift getBotNick
            case msg of
                IRC.Message (Just (IRC.NickName nick _ _)) _ param -> 
                                when (nick == U.fromString botNick) $ lift (setBotNick $ U.toString $ head param) >> answerMsg (msg {IRC.msg_command = U.fromString "PRIVMSG", IRC.msg_params = [U.fromString "#code"]}) "My nickname changed"
                _                                                  -> return ()
        | otherwise = return ()
    eval _ = return ()

