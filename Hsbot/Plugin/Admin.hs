module Hsbot.Plugin.Admin
    ( admin
    , theAdmin
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M

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
        ]

exitBot :: String -> IRC.Message -> [String] -> Plugin (Env IO) ()
exitBot exitmsg msg _ = do writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "QUIT") [U.fromString exitmsg] 
                           lift $ setGlobalQuitMVar BotExit

joinChan :: IRC.Message -> [String] -> Plugin (Env IO) ()
joinChan msg (chan:_) = writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "JOIN") [U.fromString chan]

partChan :: String -> IRC.Message -> [String] -> Plugin (Env IO) ()
partChan partmsg msg (chan:_) = writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "PART") [U.fromString chan, U.fromString partmsg]

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

