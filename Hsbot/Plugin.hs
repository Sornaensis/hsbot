module Hsbot.Plugin
    ( loadPlugin
    , sendToPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import System.IO
import System.Plugins

import Hsbot.Types
import Hsbot.Utils

-- | Loads a plugin into an ircBot
loadPlugin :: String -> IrcBot ()
loadPlugin name = do
    bot <- get
    plugin <- liftIO $ effectivelyLoadPlugin name (botChannel bot)
    case plugin of
        Just plugin' -> do
            let oldPlugins = botPlugins bot
            put $ bot { botPlugins = plugin' : oldPlugins } -- TODO : clean with a correct append
        Nothing -> return ()

-- | Effectively try to load a plugin
effectivelyLoadPlugin :: String -> Chan BotMsg -> IO (Maybe Plugin)
effectivelyLoadPlugin name serverChan = do
    -- TODO : test if Plugins/ ++ name ++ .hs exists
    --        Just load, do not compile if .o already present
    m <- liftIO $ makeAll ("Plugins/" ++ name ++ ".hs") []
    plugin <- case m of
        MakeSuccess _ _ -> do
            ldstat <- load_ ("Plugins/" ++ name ++ ".o") [".","Hsbot","Hsbot/Plugins"] ("main" ++ name)
            case ldstat of
                LoadSuccess v entryPoint -> do
                    putStrLn $ inColor ("Loaded plugin: " ++ name) [32]
                    chan <- newChan :: IO (Chan BotMsg)
                    threadId <- forkIO $ entryPoint serverChan chan
                    return $ Just (Plugin name v threadId chan)
                LoadFailure e -> do
                    putStrLn $ inColor ("Couldn't load plugin: " ++ name) [31]
                    mapM_ putStrLn e
                    return Nothing
        MakeFailure e -> do
            putStrLn $ inColor ("FATAL: Couldn't compile plugin: " ++ name) [31]
            mapM_ putStrLn e
            return Nothing
    return plugin

-- | Sends a msg to a plugin
sendToPlugin :: BotMsg -> Plugin -> IrcBot ()
sendToPlugin msg plugin = do
    let chan = pluginChannel plugin
    liftIO $ writeChan chan msg
