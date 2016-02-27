module Hsbot.Plugin.Buffer
    ( buffer
    , theBuffer
    ) where

import Control.Monad.Reader
import Data.Char
import Hsbot.Message
import Hsbot.Types
import Network.Curl
import Network.HTTP.Base (urlEncode)

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import qualified Network.IRC as IRC

-- | The Buffer plugin identity
buffer :: PluginId
buffer = PluginId
    { pluginName = "buffer"
    , pluginEp   = theBuffer
    , pluginCmds = Nothing }

-- | An IRC plugin for buffering text
theBuffer :: Plugin (Env IO) ()
theBuffer = forever $ evalStack [] 
  where
    evalStack ms = readMsg >>= \m -> if length ms > 2000
                                     then eval (m:init ms) m >> evalStack (m:init ms)
                                     else eval (m:ms) m >> evalStack (m:ms)

    eval :: [Message] -> Message -> Plugin (Env IO) ()
    eval msgStack (IncomingMsg msg)
        | IRC.msg_command msg == U.fromString "PRIVMSG" = 
                lift (getCommand msg) >>= \cmdArgs ->
                        case cmdArgs of
                            "back":[] -> case IRC.msg_prefix msg of
                                            Just (IRC.NickName n _ _) -> lift (curlStack 50 msgStack (U.toString $ head $ IRC.msg_params msg) (U.toString n)) >>= \resp ->
                                                                    case resp of
                                                                        (CurlOK,link) -> answerMsg msg link
                                                                        _             -> answerMsg msg "An error occured"
                                            _                         -> return ()
                            _ -> return ()
        | otherwise = return ()
    eval _ _ = return ()

-- | Format IRC Messages into pretty Strings
formatMsg :: Message -> String
formatMsg (IncomingMsg m) = case m of
                                IRC.Message (Just (IRC.NickName n _ _)) "PRIVMSG" [_,ps] -> case take 8 $ U.toString ps of
                                                                                              '\x01':"ACTION " -> "* " ++ U.toString n ++ " " ++ filterIRC (init (drop 8 $ U.toString ps))
                                                                                              _                -> "<" ++ U.toString n ++ "> " ++ filterIRC (U.toString ps)
                                IRC.Message (Just (IRC.NickName n _ _)) "NOTICE" (_:ps) -> "NOTICE FROM <" ++ U.toString n ++ "> " ++ concatMap U.toString ps
                                IRC.Message (Just (IRC.NickName n _ _)) "PART" (p:ps)   -> "◀ " ++ U.toString n ++ " has left " ++ U.toString p ++ ": " ++ concatMap (filterIRC . U.toString) ps
                                IRC.Message (Just (IRC.NickName n _ _)) "KICK" (p:t:ps) -> "◀ " ++ U.toString n ++ " has kicked " ++ U.toString t ++ " (" ++ concatMap (filterIRC . U.toString) ps ++ ") from " ++ U.toString p
                                IRC.Message (Just (IRC.NickName n _ _)) "MODE" (p:ps)   -> "~ " ++ " Mode " ++ U.toString p ++ " [" ++ unwords (map U.toString ps) ++ "]" ++ " by " ++ U.toString n
                                IRC.Message (Just (IRC.NickName n _ _)) "JOIN" (p:_)    -> "▶ " ++ U.toString n ++ " has joined " ++ U.toString p
                                _                                                       -> ""
formatMsg _               = ""

filterIRC :: String -> String
filterIRC = filter (\c -> (c/='\x01') && (c/='\x02') && (c/='\x03'))

-- | Invoke Curl to send the given message stack to sprunge
curlStack :: Int -> [Message] -> String -> String -> Env IO (CurlCode, String)
curlStack num msgStack chan snder = lift $ curlGetString "http://sprunge.us" (method_POST ++ [CurlPostFields ["sprunge=" ++ "Last 50 messages sent on " ++ chan ++ "\n" ++ urlEncode (unlines $ reverse (take num (filter (not . null) (map formatMsg filteredStack))))]])
                          where 
                          filteredStack = filter filterMsg msgStack
                          filterMsg   msg = case msg of
                                            (IncomingMsg m) -> case chan of
                                                                    '#':_ -> ((==chan) . U.toString . head . IRC.msg_params) m
                                                                    _     -> ((==snder) . U.toString . head . IRC.msg_params) m
                                            _               -> False
