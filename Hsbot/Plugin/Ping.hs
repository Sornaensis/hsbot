module Hsbot.Plugin.Ping
    ( ping
    , thePing
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types

import qualified Data.ByteString.Char8 as S

-- | The ping plugin identity
ping :: PluginId
ping = PluginId
    { pluginName = "ping"
    , pluginEp   = thePing }

-- | An IRC plugin that answer PING requests
thePing :: Plugin (Env IO) ()
thePing = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == (S.pack "PING") = writeMsg . OutgoingMsg . IRC.Message Nothing (S.pack "PONG") $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

