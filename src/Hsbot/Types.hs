module Hsbot.Types
    ( AccessList (..)
    , AccessRight (..)
    , adminAccess
    , operatorAccess
    , halfopAccess
    , userAccess 
    , Bot
    , BotState (..)
    , BotStatus (..)
    , BotEnv (..)
    , Config (..)
    , Env
    , Message (..)
    , Plugin
    , PluginEnv (..)
    , PluginCmds
    , PluginCmd (..)
    , PluginRet 
    , PluginId (..)
    , TLSConfig (..)
    ) where

import Data.Dynamic  
import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Network
import qualified Network.IRC as IRC
import Network.TLS
import System.IO

-- The bot environment
type Env = ReaderT BotEnv

data BotEnv = BotEnv
    { envBotState    :: MVar BotState
    , envHandle      :: Handle
    , envChan        :: Chan Message
    , envQuitMv      :: MVar BotStatus
    , envThreadIdsMv :: MVar [ThreadId]
    , envConfig      :: Config
    , envTLS         :: Maybe ClientParams
    , envTLSCtx      :: Maybe Context
    }

-- The bot monad
type Bot = StateT BotState

data BotState = BotState
    { botPlugins  :: M.Map String (PluginEnv, ThreadId)
    , botAccess   :: [AccessList]
    , botHooks    :: [Chan Message]
    , botChannels :: [String]
    , botNickname :: String
    , botPrefix   :: Maybe String
    }

-- The Plugin monad
type Plugin = ReaderT PluginEnv

data PluginEnv = PluginEnv
    { pluginId     :: PluginId
    , pluginChan   :: Chan Message
    , pluginMaster :: Chan Message
    }

data PluginId = PluginId
    { pluginName :: String
    , pluginEp   :: Plugin (Env IO) ()
    , pluginCmds :: Maybe PluginCmds
    }


-- Plugin help system
type PluginCmds = [PluginCmd]

type PluginRet = Dynamic

data PluginCmd = PluginCmd 
    { command     :: String
    , cmdIsPrefix :: Bool
    , cmdFunc     :: IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
    , cmdPerm     :: Maybe AccessRight
    , cmdHelp     :: String
    } 

data AccessRight = Admin 
                 | Operator 
                 | HalfOp 
                 | User 
                 deriving (Eq, Show, Ord)

adminAccess :: [AccessRight]
adminAccess = [Admin, Operator, HalfOp, User]

operatorAccess :: [AccessRight]
operatorAccess = [Operator, HalfOp, User]

halfopAccess :: [AccessRight]
halfopAccess = [HalfOp, User]

userAccess :: [AccessRight]
userAccess = [User]

-- Messaging
data Message = IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

data BotStatus = BotExit | BotReload String | BotRestart (String, Maybe String) deriving (Read, Show)

-- Config
data Config = Config
    { configErrors    :: Maybe String
    , configCmdPrefix :: Maybe String
    , configTLS       :: TLSConfig
    , configAddress   :: String
    , configPort      :: PortID
    , configPassword  :: Maybe String
    , configAccess    :: [AccessList]
    , configChannels  :: [String]
    , configNicknames :: [String]
    , configRealname  :: String
    , configPlugins   :: [PluginId]
    }

data AccessList = AccessList
    { accessMask :: IRC.Prefix
    , accessList :: [AccessRight]
    } deriving (Show)

data TLSConfig = TLSConfig
    { sslOn       :: Bool
    , sslVersions :: [Network.TLS.Version]
    , sslCiphers  :: [Network.TLS.Cipher]
    , sslVerify   :: Bool
    , sslLogging  :: Logging
    }

