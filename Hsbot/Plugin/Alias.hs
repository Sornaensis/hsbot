{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Hsbot.Plugin.Alias
    ( alias
    , theAlias
    ) where

import Control.Concurrent.Chan ()
import Control.Concurrent 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Data.Acid
import Data.Char
import Data.SafeCopy
import Data.Typeable
import Data.List.Split
import System.Environment.XDG.BaseDir

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U
import qualified Network.IRC as IRC
import qualified Data.List as L
import qualified Data.Map as M

type Key = String
type Value = String

data BotAlias = BotAlias !(M.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''BotAlias)

makeAlias :: Key -> Value -> Update BotAlias ()
makeAlias key val = do
    BotAlias m <- get
    put (BotAlias (M.insert key val m))

deleteAlias :: Key -> Update BotAlias ()
deleteAlias key = do
    BotAlias m <- get
    put (BotAlias (M.delete key m))

getAlias :: Key -> Query BotAlias (Maybe Value)
getAlias key = do
    BotAlias m <- ask
    return (M.lookup key m)

$(makeAcidic ''BotAlias ['makeAlias, 'getAlias, 'deleteAlias])

-- | The Alias plugin identity
alias :: PluginId
alias = PluginId
    { pluginName = "alias"
    , pluginEp   = theAlias 
    , pluginCmds = Nothing }

-- | An IRC plugin for aliasing text
theAlias :: Plugin (Env IO) ()
theAlias = do
    baseDir <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot"
    aliasDB <- lift getAliasDB >>= \dbName -> liftIO $ openLocalStateFrom (baseDir ++ "/" ++ dbName ++ "/aliasDB/") (BotAlias M.empty)
    forever $ readMsg >>= eval aliasDB
  where
    eval :: AcidState BotAlias -> Message -> Plugin (Env IO) ()
    eval aliasDB (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" = case IRC.msg_prefix msg of 
                   Just (IRC.NickName nick (Just user) (Just server)) -> 
                       case "bot" `L.isSuffixOf` (map toLower $ U.toString nick) of
                            True -> return ()
                            False -> do
                                cmdArgs <- lift $ getCommand msg
                                case cmdArgs of
                                    "alias":name:"=":[]   -> do
                                                             liftIO $ update aliasDB (DeleteAlias name)
                                    "alias":name:"=":msgs -> do
                                                             let amsg = unwords msgs
                                                             answerMsg msg $ "Added alias " ++ boldText name ++ " -> " ++ "\'" ++ amsg ++ "\'"
                                                             liftIO $ update aliasDB (MakeAlias name amsg)
                                    "alias":[] -> answerMsg msg $ "Syntax: alias <name> = <text>"
                                    "whatis":[] -> answerMsg msg "Syntax: whatis <name>"
                                    "whatis":name:[] -> do
                                                     aval <- liftIO $ query aliasDB (GetAlias name)
                                                     case aval of
                                                        Just [] -> return ()
                                                        Just txt -> answerMsg msg txt
                                                        Nothing  -> return ()
                                    akey:args  -> do
                                                  aval <- liftIO $ query aliasDB (GetAlias akey)
                                                  case aval of
                                                      Just []  -> return ()
                                                      
                                                      Just txt -> do
                                                                  let (a:as) = words txt
                                                                  case a of
                                                                      '\\':name -> do
                                                                                   aval' <- liftIO $ query aliasDB (GetAlias name)
                                                                                   case aval' of
                                                                                       Just [] -> return ()
                                                                                       Nothing -> return ()
                                                                                       Just sub -> answerMsg msg $ formatAlias (formatAlias sub as) args
                                                                      _ -> answerMsg msg $ formatAlias txt args
                                                      Nothing  -> return ()
                                    _ -> return ()
                   _ -> return ()
        | otherwise = return ()
    eval _ _ = return ()

getAliasDB :: Env IO String
getAliasDB = getBotNick >>= \n -> getBotServer >>= \s -> return $ n ++ "-" ++ s

formatAlias :: String -> [String] -> String
formatAlias s []   = s
formatAlias s argl = concat $ combine (splitOn "{}" s) argl
                     where
                     combine :: [String] -> [String] -> [String]
                     combine []    _      = []
                     combine (f:n) []     = f:n
                     combine (f:n) (a:as) = [f,a] ++ combine n as 
