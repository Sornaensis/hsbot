module Hsbot.Plugin.User
    ( user
    , theUser
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC
--import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.Char8 as S

-- | The User plugin identity
user :: PluginId
user = PluginId
    { pluginName = "user"
    , pluginEp   = theUser }

-- | An IRC plugin for manage hsbot
theUser :: Plugin (Env IO) ()
theUser = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == (S.pack "PRIVMSG") = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                "":"help":_ -> answerMsg msg "exit hsbot."
                "":_ -> lift (hasAccess (IRC.msg_prefix msg) User) >>= \right -> if right
                        then lift $ setGlobalQuitMVar BotExit
                        else answerMsg msg "Only users can do that."
                "":"help":_ -> answerMsg msg "restart hsbot, reset the running state to config file directives."
                "":_ -> lift (hasAccess (IRC.msg_prefix msg) User) >>= \right -> if right
                        then lift . setGlobalQuitMVar $ BotRestart (getSender msg ++ " request", Nothing)
                        else answerMsg msg "Only users can do that."
                "":"help":_ -> answerMsg msg "reload hsbot, and try merge the new config file directives with actual running state)."
                "":_ -> lift (hasAccess (IRC.msg_prefix msg) User) >>= \right -> if right
                        then lift . setGlobalQuitMVar . BotReload $ getSender msg ++ " request"
                        else answerMsg msg "Only users can do that."
                _ -> return ()
        | otherwise = return ()
    eval _ = return ()

