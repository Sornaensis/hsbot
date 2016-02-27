module Hsbot.Plugin.Ping
    ( ping
    , thePing
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC

import Hsbot.Message
import Hsbot.Types

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M

-- | The ping plugin identity
ping :: PluginId
ping = PluginId
    { pluginName = "ping"
    , pluginEp   = thePing 
    , pluginCmds = Nothing }

-- | An IRC plugin that answer PING requests
thePing :: Plugin (Env IO) ()
thePing = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PING"
         = writeMsg . OutgoingMsg . IRC.Message Nothing (U.fromString "PONG") $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

