module Hsbot.Plugin.CTCP
    ( ctcp
    , theCtcp
    , CtcpConfig (..)
    , defCtcp
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import Data.Char 
import qualified Network.IRC as IRC
import qualified Data.List as L

import Hsbot.Message hiding (getCommand)
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M

import Control.Monad 
import Control.Monad.Error 
import Data.IORef 
import System.Environment 
import System.IO

-- | The hsch plugin identity
ctcp :: PluginId
ctcp = PluginId
    { pluginName = "CTCP"
    , pluginEp   = theCtcp defCtcp
    , pluginCmds = Just (theCmds defCtcp) 
    }

data CtcpConfig = CtcpConfig 
                { versionReply :: String
                , avatarReply  :: String
                , fingerReply  :: String }

defCtcp :: CtcpConfig 
defCtcp = CtcpConfig 
        { versionReply = "hsbot2 -- A Haskell IRC Bot" 
        , avatarReply  = "http://i.imgur.com/21vrxqB.jpg"
        , fingerReply  = ":^)" }

theCmds :: CtcpConfig -> PluginCmds
theCmds config = [ 
            PluginCmd
            { command     = "\SOHVERSION"
            , cmdIsPrefix = True
            , cmdFunc     = replyCtcp . ctcpMsg $ "VERSION " ++ versionReply config
            , cmdPerm     = Nothing
            , cmdHelp     = "Version Reply"
            }
          , PluginCmd
            { command     = "\SOHAVATAR"
            , cmdIsPrefix = True
            , cmdFunc     = replyCtcp . ctcpMsg $ "AVATAR " ++ avatarReply config
            , cmdPerm     = Nothing
            , cmdHelp     = "Version Reply"
            }
          , PluginCmd
            { command     = "\SOHFINGER"
            , cmdIsPrefix = True
            , cmdFunc     = replyCtcp . ctcpMsg $ "FINGER " ++ fingerReply config
            , cmdPerm     = Nothing
            , cmdHelp     = "Version Reply"
            }
          ]

getCommand :: IRC.Message -> Env IO [String]
getCommand = getCommandNoFilter

-- | An IRC plugin for hsching text
theCtcp :: CtcpConfig -> Plugin (Env IO) ()
theCtcp config  = forever $ readMsg >>= eval 
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg) 
        | IRC.msg_command msg == U.fromString "PRIVMSG" =
                   lift (getCommand msg) >>= 
                   \cmdArgs -> void $ mapM_ (parseCommand msg cmdArgs) (theCmds config)
        | otherwise = return ()
    eval _ = return ()

replyCtcp :: String -> IRC.Message -> [String] -> Plugin (Env IO) ()
replyCtcp reply (IRC.Message (Just (IRC.NickName nick _ _)) _ _) _ = 
    writeMsg . OutgoingMsg $ IRC.Message Nothing (U.fromString "NOTICE") [nick, U.fromString reply]


