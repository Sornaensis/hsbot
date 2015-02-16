module Hsbot.Plugin.Mueval
    ( mueval
    , theMueval
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import Control.Monad.State
import qualified Network.IRC as IRC

import Control.Concurrent (forkIO, threadDelay)
import GHC.IO.Handle (hGetContents)
import System.Exit (exitWith, ExitCode(..))
import System.Posix.Signals (signalProcess)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), getProcessExitCode, terminateProcess)
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import System.Environment.XDG.BaseDir

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U

-- | The plugin identity
mueval :: PluginId
mueval = PluginId
    { pluginName = "haskell-eval"
    , pluginEp   = theMueval  
    , pluginCmds = Just theCmds }

type MuState = StateT Mueval

data Mueval = Mueval 
    { letDeclarations :: [String] }

theCmds :: PluginCmds
theCmds = [
            PluginCmd
            { command = "λ"
            , cmdIsPrefix = True
            , cmdFunc = evalLambda
            , cmdPerm = Nothing
            , cmdHelp = "Safely evaluate pure haskell code / SYNTAX: λ [HASKELL EXPRESSION]"
            }
        ]

evalLambda :: IRC.Message -> [String] -> Plugin (Env IO) ()
evalLambda msg args = 
        liftIO (do 
                   egoBotDefFile <- liftIO (System.Environment.XDG.BaseDir.getUserDataDir "hsbot") 
                   (_,Just hout,_,hdl) <- createProcess (proc "mueval-core" ["-l", egoBotDefFile ++ "/mueval/EgoBot.hs", "-e",unwords args]){std_out = CreatePipe}
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
                   hGetContents hout >>= return) >>= mapM_ (answerMsg msg) . map (\x -> if length x > 300 then take 300 x ++ "..." else x) . take 2 . lines


-- | An IRC plugin for manage hsbot
theMueval :: Plugin (Env IO) ()
theMueval = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" = 
            do
                cmdArgs <- lift $ getCommand msg
                void $ mapM_ (parseCommand msg cmdArgs) theCmds 
        | otherwise = return ()
    eval _ = return ()


