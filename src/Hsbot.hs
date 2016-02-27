module Hsbot
    ( hsbot
    , hsbot'
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.Reader
import qualified Data.Map as M
import System.Log.Logger

import System.Environment.XDG.BaseDir

import Hsbot.Core
import Hsbot.Types

data State = State (M.Map String String) deriving (Read, Show)

startHsbot :: Config -> IO ()
startHsbot config = do
    (State buffer) <- restoreTextState $ State M.empty
    -- checking for configuration file compilation error
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    -- Handle previous exit state if it exists
    dieMsgs <- case M.lookup "die_msg" buffer of
        Just dieMsg -> case reads dieMsg :: [(BotStatus, String)] of
            (status, _):_ -> case status of
                BotReload reason -> return ["Bot reloaded, reason : " ++ reason]
                BotRestart (reason, Just info) -> return ["Bot restarted, reason : " ++ reason, "additional information: " ++ info]
                BotRestart (reason, Nothing) -> return ["Bot restarted, reason : " ++ reason]
                BotExit -> return []
            _ -> return ["hsbot die_msg parsing error, this should not happen"]
        Nothing -> return []
    mapM_ (infoM "Hsbot") dieMsgs
    -- initialization
    infoM "Hsbot" "Bot initializations"
    hsbotEnv <- initHsbot config
    -- main stuff
    infoM "Hsbot" "Bot core starting"
    status <- runReaderT (runHsbot dieMsgs) hsbotEnv
    infoM "Hsbot" $ "Bot core exited with status " ++ show status
    -- Handling exit signal
    case status of
         BotExit -> runReaderT terminateHsbot hsbotEnv
         BotReload reason -> do
             runReaderT terminateHsbot hsbotEnv
             relaunchWithTextState (State $ M.singleton "die_msg" . show $ BotReload reason) Nothing  -- TODO find a way to properly implement that, then insert necessary information in this MVar
         BotRestart reason -> do
             runReaderT terminateHsbot hsbotEnv
             relaunchWithTextState (State $ M.singleton "die_msg" . show $ BotRestart reason) Nothing

hsbot' :: Config -> IO ()
hsbot' = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsbot"
    , Dyre.configDir   = Just $ System.Environment.XDG.BaseDir.getUserConfigDir "hsbot" 
    , Dyre.realMain    = startHsbot
    , Dyre.ghcOpts     = ["-dynamic"]
    , Dyre.showError   = \config err -> config { configErrors = Just err } }

hsbot :: String -> Config -> IO ()
hsbot []      = hsbot'
hsbot appName = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = appName
    , Dyre.configDir   = Just $ System.Environment.XDG.BaseDir.getUserConfigDir "hsbot" 
    , Dyre.realMain    = startHsbot
    , Dyre.ghcOpts     = ["-dynamic"]
    , Dyre.showError   = \config err -> config { configErrors = Just err } }
