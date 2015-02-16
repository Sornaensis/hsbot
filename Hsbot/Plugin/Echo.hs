module Hsbot.Plugin.Echo
    ( echo
    , theEcho
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import Data.Char 
import qualified Network.IRC as IRC
import qualified Data.List as L

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M

-- | The Echo plugin identity
echo :: PluginId
echo = PluginId
    { pluginName = "echo"
    , pluginEp   = theEcho 
    , pluginCmds = Nothing }

-- | An IRC plugin for echoing text
theEcho :: Plugin (Env IO) ()
theEcho = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" =  case IRC.msg_prefix msg of 
               Just (IRC.NickName nick (Just user) (Just server)) -> 
                   case "bot" `L.isSuffixOf` (map toLower $ U.toString nick) of
                        True -> return ()
                        False ->do
                            cmdArgs <- lift $ getCommand msg
                            case cmdArgs of
                                "action":('#':chan):msgs -> lift (hasAccess (IRC.msg_prefix msg) $ Just Admin) >>=
                                        \right           -> if right
                                        then case msg of
                                             (IRC.Message a b (_:p)) -> answerMsg (IRC.Message a b (U.fromString ('#':chan):p)) $ "\x01" ++ "ACTION " ++ unwords msgs ++ "\x01"
                                             IRC.Message _ _ []      -> answerMsg msg "What?"
                                        else answerMsg msg "Only admins can do that."
                                "action":msgs          -> answerMsg msg $ "\x01" ++ "ACTION " ++ unwords msgs ++ "\x01"
                                "echo":('#':chan):msgs -> lift (hasAccess (IRC.msg_prefix msg) $ Just Admin) >>=
                                        \right         -> if right
                                        then case msg of
                                             (IRC.Message a b (_:p)) -> answerMsg (IRC.Message a b (U.fromString ('#':chan):p)) $ unwords msgs
                                             IRC.Message _ _ []      -> answerMsg msg "What?"
                                        else answerMsg msg "Only admins can do that."
                                "echo":msgs    -> answerMsg msg $ unwords msgs
                                "ping":_       -> answerMsg msg "Pong!"
                                "debug":msgs   -> answerMsg msg $ show msg ++ " => " ++ show msgs
                                "botnick":_ -> lift getBotNick >>= \n                                                       
                                                -> lift getBotServer >>= \s -> answerMsg msg $ "I am " ++ n ++ " on " ++ s
                                "g":query   -> answerMsg msg $ "https://www.google.com/search?q=" ++ L.intercalate "+" query
                                "google":query -> answerMsg msg $ "https://www.google.com/search?q=" ++ L.intercalate "+" query
                                _              -> return ()
               _ -> return ()
        | otherwise = return ()
    eval _ = return ()
