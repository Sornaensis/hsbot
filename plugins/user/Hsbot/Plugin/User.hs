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

import qualified Data.ByteString.Char8 as S

-- | The User plugin identity
user :: PluginId
user = PluginId
    { pluginName = "user"
    , pluginEp   = theUser 
    , pluginCmds = Just theCmds}

theCmds :: PluginCmds
theCmds = []

-- | An IRC plugin for manage hsbot
theUser :: Plugin (Env IO) ()
theUser = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == (S.pack "PRIVMSG") = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                _ -> return ()
        | otherwise = return ()
    eval _ = return ()

