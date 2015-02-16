module Hsbot.Plugin.Help
    ( help
    , theHelp
    ) where

import Control.Concurrent 
import Control.Concurrent.Chan ()
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import qualified Network.IRC as IRC

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import qualified Data.List as L

-- | The Help plugin identity
help :: PluginId
help = PluginId
    { pluginName = "help"
    , pluginEp   = theHelp 
    , pluginCmds = Just theCmds }

theCmds :: PluginCmds
theCmds = [
            PluginCmd
            { command     = "list"
            , cmdIsPrefix = False
            , cmdFunc     = listPlugin
            , cmdPerm     = Nothing
            , cmdHelp     = "List loaded plugins or plugin topics / list <plugin>"
            }
          , PluginCmd
            { command     = "help"
            , cmdIsPrefix = False
            , cmdFunc     = listPlugin
            , cmdPerm     = Nothing
            , cmdHelp     = "List loaded plugins or plugin topics / list <plugin>"
            }
          ]

listPlugin :: IRC.Message -> [String] -> Plugin (Env IO) ()
listPlugin msg []       = lift pluginList >>= \m -> answerMsg msg $ ("Loaded Plugins: " ++ L.intercalate ", " m) ++ " / Use list <plugin> to see plugin topics"
listPlugin msg (p:c:[]) = lift pluginGetHelp >>= \m -> 
                                  answerMsg msg $ L.intercalate " / " $ map getHelpTxt $ map (fromJust . pluginCmds) $ filter ((==p) . pluginName) $ filter (isJust . pluginCmds) m
                                  where
                                  getHelpTxt []     = "No help available for " ++ p
                                  getHelpTxt (x:xs) = if command x == c 
                                                        then cmdHelp x
                                                        else getHelpTxt xs
listPlugin msg (p:_)    = lift pluginGetHelp >>= \m ->
                                   searchPlugins $ map (fromJust . pluginCmds) $ filter ((==p) . pluginName) $ filter (isJust . pluginCmds) m
                                   where
                                   searchPlugins []  = lift pluginGetHelp >>= \m ->
                                                       reply $ filter (not.null.snd) $ map getHelpTxt $ map (\pid -> (pid, fromJust (pluginCmds pid))) $ filter (isJust . pluginCmds) m
                                                       where
                                                       reply [] = answerMsg msg $ "No help available for " ++ p
                                                       reply hs = if length hs > 1 
                                                                    then answerMsg msg $ "Command "++p++" found in plugins: "++(L.intercalate ", " $ map fst hs)
                                                                    else answerMsg msg . snd $ head hs
                                                       getHelpTxt (pid,[])   = (pluginName pid,[])
                                                       getHelpTxt (pid,x:xs) = if command x == p 
                                                                               then (pluginName pid, cmdHelp x)
                                                                               else getHelpTxt (pid,xs)
                                   searchPlugins xs  = mapM_ getPluginTopics xs
                                   getPluginTopics []      = answerMsg msg $ boldText p ++ " has no topics"
                                   getPluginTopics (x:xs)  = answerMsg msg $ boldText p ++ " topics: " ++ (L.intercalate ", " $ getCmdsCmds (x:xs)) 
                                                             where 
                                                             getCmdsCmds []     = []
                                                             getCmdsCmds (x:xs) = command x : getCmdsCmds xs

-- | An IRC plugin for getting help
theHelp :: Plugin (Env IO) ()
theHelp = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" = do
            cmdArgs <- lift $ getCommand msg
            mapM_ (parseCommand msg cmdArgs) theCmds >> return ()
        | otherwise = return ()
    eval _ = return ()
    
-- | Get the list of Plugin Names from the BotState
pluginList :: Env IO [String]
pluginList = asks envBotState >>= liftIO .readMVar >>= evalStateT (fmap M.keys (gets botPlugins)) 

pluginGetHelp :: Env IO [PluginId]
pluginGetHelp = asks envBotState >>= liftIO . readMVar >>= evalStateT (fmap M.elems (gets botPlugins)) >>= \ps -> return $ map (pluginId . fst) ps
