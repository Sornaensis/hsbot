module Hsbot.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , parseCommand
    , getBotNick 
    , setBotNick
    , getBotServer 
    , hasAccess
    , boldText 
    , ctcpMsg
    , initTLSEnv
    , sendStr
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Char8 as S
import qualified Data.List as L
import qualified Network.IRC as IRC
import Network.TLS
import System.IO

import Hsbot.Types

-- | Parse a command from an IRC.Message using a PluginCmd
parseCommand :: IRC.Message -> [String] -> PluginCmd -> Plugin (Env IO) ()
parseCommand msg (arg:args) (PluginCmd cmd isPrefix func permission _) =
    lift (hasAccess (IRC.msg_prefix msg) permission) >>= \access ->
    case access of
        True -> case isPrefix of
                    True -> if cmd `L.isPrefixOf` arg then func msg ((L.drop (L.length cmd) arg) : args) else return ()
                    _    -> if cmd == arg then func msg args else return ()
        _    -> return ()

-- utility functions
getBotNick :: Env IO String
getBotNick = asks envBotState >>= liftIO . readMVar >>= evalStateT (gets botNickname)

setBotNick :: String -> Env IO ()
setBotNick nick = asks envBotState >>= 
                \state -> 
                liftIO $ modifyMVar_ state (\s -> return $ s {botNickname = nick})   

getBotServer :: Env IO String
getBotServer = asks envConfig >>= evalStateT (gets configAddress)

addThreadIdToQuitMVar :: ThreadId -> Env IO ()
addThreadIdToQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ thrId:l)

delThreadIdFromQuitMVar :: ThreadId -> Env IO ()
delThreadIdFromQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (return . L.delete thrId)

setGlobalQuitMVar :: BotStatus -> Env IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

-- Access rights
hasAccess :: Maybe IRC.Prefix -> Maybe AccessRight -> Env IO Bool
-- If no access is required then access is granted
hasAccess _ Nothing = return True
hasAccess Nothing _ = return False
hasAccess (Just mask) (Just right) =
    asks envBotState >>= liftIO . readMVar >>= evalStateT (fmap (any accessMatch) (gets botAccess))
  where
    accessMatch :: AccessList -> Bool
    accessMatch (AccessList amask arights)
      | mask == amask = (right `L.elem` arights)
      | otherwise = False

-- | IRC Message utilities
boldText :: String -> String
boldText xs = "\x02" ++ xs ++ "\x02"

ctcpMsg :: String -> String
ctcpMsg xs = "\SOH" ++ xs ++ "\SOH"

-- Helpers
sendStr :: BotEnv -> Handle -> Maybe Context -> String -> IO ()
sendStr env _ (Just ctx) msg = sendData ctx (U.fromString $ msg ++ "\r\n") `catch` handleIOException env ("sendStr " ++ msg)
sendStr env handle Nothing msg = hPutStrLn handle (msg ++ "\r\n") `catch` handleIOException env ("sendStr " ++ msg)

handleIOException :: BotEnv -> String -> IOException -> IO ()
handleIOException env msg ioException = do
    runReaderT (setGlobalQuitMVar $ BotRestart (show ioException, Just msg)) env
    myId <- myThreadId
    killThread myId
    return ()

-- TLS utils
initTLSEnv :: Config -> IO ClientParams
initTLSEnv config = do
    let versions = sslVersions (configTLS config)
        ciphers  = sslCiphers  (configTLS config)
    return ClientParams {  
                clientUseMaxFragmentLength    = Nothing
               ,clientServerIdentification    = (configAddress config, S.pack "") 
               ,clientUseServerNameIndication = False
               ,clientWantSessionResume       = Nothing
               ,clientShared                  = def Shared
               ,clientHooks                   = ClientHooks {
                                                  onCertificateRequest = onCertificateRequest (def ClientHooks)
                                                 ,onNPNServerSuggest   = onNPNServerSuggest   (def ClientHooks)
                                                 ,onServerCertificate  = if sslVerify $ configTLS config then def else onCertTLSno
                                                }
               ,clientSupported               = Supported {
                                                  supportedVersions             = versions
                                                 ,supportedCiphers              = ciphers
                                                 ,supportedCompressions         = [nullCompression]
                                                 ,supportedHashSignatures       = supportedHashSignatures (def Supported) 
                                                 ,supportedSecureRenegotiation  = True
                                                 ,supportedSession              = True
                                                }
            }


onCertTLSno :: Monad m => t -> t1 -> t2 -> t3 -> m [t4]
onCertTLSno _ _ _ _ = return []
