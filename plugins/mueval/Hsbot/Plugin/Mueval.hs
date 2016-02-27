{-# LANGUAGE DeriveDataTypeable #-}
module Hsbot.Plugin.Mueval
    ( mueval
    , theMueval
    ) where

import Control.Concurrent.Chan ()
import Control.Monad
import Control.Monad.Reader
import Data.Dynamic
import qualified Network.IRC as IRC

import Control.Concurrent (forkIO, threadDelay)
import GHC.IO.Handle (hGetContents)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath.Posix ((</>))
import System.Posix.Signals (signalProcess)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), getProcessExitCode, terminateProcess, waitForProcess)
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import System.Environment.XDG.BaseDir

import Hsbot.Message hiding (getCommand)
import Hsbot.Types
import Hsbot.Utils 

import qualified Data.ByteString.UTF8 as U

getCommand :: IRC.Message -> Env IO [String]
getCommand = getCommandNoFilter

-- | The plugin identity
mueval :: PluginId
mueval = PluginId
    { pluginName = "mueval"
    , pluginEp   = theMueval 
    , pluginCmds = Just $ theCmds [] }

theCmds ::[String] -> PluginCmds
theCmds lets = [ PluginCmd
            { command = ">>"
            , cmdIsPrefix = True
            , cmdFunc = evalLambda lets
            , cmdPerm = Nothing
            , cmdHelp = "Safely evaluate pure haskell code / SYNTAX: >> [HASKELL EXPRESSION]"
            }, 
                PluginCmd
            { command = ":t"
            , cmdIsPrefix = True
            , cmdFunc = evalType lets 
            , cmdPerm = Nothing
            , cmdHelp = "Safely get type signature from a haskell expression / SYNTAX: :t [HASKELL EXPRESSION]"
            }
            ]

data LambdaRet = NoLets | Lets [String] deriving Typeable

evalLambda :: [String] -> IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
evalLambda _ _ [] = return (Just (toDyn NoLets)) 
evalLambda _ _ ["let"] = return (Just (toDyn NoLets)) 
evalLambda lets msg ("let":args) = 
        liftIO (do egoBotDefFile <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot" 
                   (_,_,_,hdl) <- createProcess 
                                            (proc "mueval-core" 
                                                    [ "-l"
                                                    , egoBotDefFile </> "mueval/EgoBot.hs"
                                                    , "-e"
                                                    , unwords lets ++ " let " ++ unwords args ++ " in show \'a\'"]){std_out = CreatePipe}
                   _ <- forkIO $ do
                            threadDelay (7 * 700000)
                            status <- getProcessExitCode hdl
                            case status of 
                                Nothing -> do terminateProcess hdl
                                              _ <- withProcessHandle hdl (\x -> case x of 
                                                                             OpenHandle pid -> signalProcess 9 pid >> return (undefined, undefined)
                                                                             _ -> return (undefined,undefined))
                                              exitWith (ExitFailure 1)
                                Just a -> exitWith a
                   waitForProcess hdl) >>= \e -> case e of
                                                             ExitSuccess   -> return (Just (toDyn (Lets $ lets ++ ["let " ++ unwords args ++ " in"])))
                                                             ExitFailure _ -> evalLambda lets msg (" let":args)
evalLambda lets msg args = 
        liftIO (do egoBotDefFile <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot" 
                   (_,Just hout,_,hdl) <- createProcess 
                                            (proc "mueval-core" 
                                                    [ "-l"
                                                    , egoBotDefFile </> "mueval/EgoBot.hs"
                                                    , "-n"
                                                    , "-e"
                                                    , unwords lets ++ " " ++ unwords args]){std_out = CreatePipe}
                   _ <- forkIO $ do
                            threadDelay (7 * 700000)
                            status <- getProcessExitCode hdl
                            case status of 
                                Nothing -> do terminateProcess hdl
                                              _ <- withProcessHandle hdl (\x -> case x of 
                                                                             OpenHandle pid -> signalProcess 9 pid >> return (undefined, undefined)
                                                                             _ -> return (undefined,undefined))
                                              exitWith (ExitFailure 1)
                                Just a -> case a of
                                            ExitSuccess   -> exitWith a
                                            ExitFailure _ -> exitWith a
                   hGetContents hout ) >>= mapM_ (answerMsg msg . (\x -> if length x > 500 then take 500 x ++ "..." else x)) . take 2 . lines 
                                                 >> return (Just (toDyn NoLets))

evalType :: [String] -> IRC.Message -> [String] -> Plugin (Env IO) (Maybe PluginRet)
evalType _ _ [] = return (Just (toDyn NoLets)) 
evalType _ _ ["let"] = return (Just (toDyn NoLets)) 
evalType lets msg ("let":args) = 
        liftIO (do egoBotDefFile <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot" 
                   (_,_,_,hdl) <- createProcess 
                                            (proc "mueval-core" 
                                                    [ "-l"
                                                    , egoBotDefFile </> "mueval/EgoBot.hs"
                                                    , "-i"
                                                    , "-T"
                                                    , "-e"
                                                    , unwords lets ++ " let " ++ unwords args ++ " in show \'a\'"]){std_out = CreatePipe}
                   _ <- forkIO $ do
                            threadDelay (7 * 700000)
                            status <- getProcessExitCode hdl
                            case status of 
                                Nothing -> do terminateProcess hdl
                                              _ <- withProcessHandle hdl (\x -> case x of 
                                                                             OpenHandle pid -> signalProcess 9 pid >> return (undefined, undefined)
                                                                             _ -> return (undefined,undefined))
                                              exitWith (ExitFailure 1)
                                Just a -> exitWith a
                   waitForProcess hdl) >>= \e -> case e of
                                                             ExitSuccess   -> return (Just (toDyn (Lets $ lets ++ ["let " ++ unwords args ++ " in"])))
                                                             ExitFailure _ -> evalType lets msg (" let":args)
evalType lets msg args = 
        liftIO (do egoBotDefFile <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot" 
                   (_,Just hout,_,hdl) <- createProcess 
                                            (proc "mueval-core" 
                                                    [ "-l"
                                                    , egoBotDefFile </> "mueval/EgoBot.hs"
                                                    , "-i"
                                                    -- , "-T "
                                                    , "-e"
                                                    , unwords lets ++ " " ++ unwords args]){std_out = CreatePipe}
                   _ <- forkIO $ do
                            threadDelay (7 * 700000)
                            status <- getProcessExitCode hdl
                            case status of 
                                Nothing -> do terminateProcess hdl
                                              _ <- withProcessHandle hdl (\x -> case x of 
                                                                             OpenHandle pid -> signalProcess 9 pid >> return (undefined, undefined)
                                                                             _ -> return (undefined,undefined))
                                              exitWith (ExitFailure 1)
                                Just a -> case a of
                                            ExitSuccess   -> exitWith a
                                            ExitFailure _ -> exitWith a
                   hGetContents hout ) >>= mapM_ (answerMsg msg . (\x -> if length x > 500 then take 500 x ++ "..." else x)) . tail . take 2 . lines 
                                                 >> return (Just (toDyn NoLets))


-- | Plugin loop
theMueval :: Plugin (Env IO) ()
theMueval = forever $ eval []
    where
    eval :: [String] -> Plugin (Env IO) ()
    eval lets = readMsg >>= eval' lets >>= \xs -> 
                case xs of
                    []    -> eval lets
                    (x:_) -> case x of
                               (Just l) -> case fromDynamic l :: Maybe LambdaRet of
                                                      Just (Lets ls) -> if length ls > 200 then eval $ tail ls else eval ls
                                                      _       -> eval lets
                               _      -> eval lets 
    eval' :: [String] -> Message -> Plugin (Env IO) [Maybe PluginRet]
    eval' lets (IncomingMsg msg)
         | IRC.msg_command msg == U.fromString "PRIVMSG" = 
             do cmdArgs <- lift $ getCommand msg
                mapM (parseCommand msg cmdArgs) $ theCmds lets
         | otherwise = return []
    eval' _ _ = return []


