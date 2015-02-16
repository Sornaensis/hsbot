module Hsbot.Message
    ( answerMsg
    , getChannel
    , getCommand
    , getCommandFilter
    , getCommandNoFilter 
    , getDestination
    , getSender
    , readMsg
    , writeMsg
    ) where

import Control.Concurrent
import qualified Data.List as L
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Data.Maybe 
import Data.ByteString.UTF8 as U

import Hsbot.Types


-- Plugin Utils
readMsg :: Plugin (Env IO) Message
readMsg = asks pluginChan >>= liftIO . readChan

-- TODO : remove the Plugin layer since it's useless, and use the envChan instead
writeMsg :: Message -> Plugin (Env IO) ()
writeMsg msg = asks pluginMaster >>= liftIO . flip writeChan msg

answerMsg :: IRC.Message -> String -> Plugin (Env IO) ()
answerMsg _ [] = return ()
answerMsg (IRC.Message (Just (IRC.NickName nick _ _)) cmd (channel:_)) msg
    | head (U.toString channel) == '#' = writeMsg . OutgoingMsg $ IRC.Message Nothing cmd [channel, U.fromString msg]
    | otherwise = writeMsg . OutgoingMsg $ IRC.Message Nothing cmd [nick, U.fromString msg]
answerMsg _ _ = return ()

-- | Get the channel a message has been posted on
getChannel :: IRC.Message -> String
getChannel (IRC.Message _ _ (channel:_)) = U.toString channel
getChannel _ = ""

-- | Get the command in the IRC message if there is one
getCommand :: IRC.Message -> Env IO [String]
getCommand = getCommandFilter

getCommandFilter :: IRC.Message -> Env IO [String]
getCommandFilter (IRC.Message _ _ [_,msg]) = getCommandFrom $ words $ U.toString msg
  where
    getCommandFrom :: [String] -> Env IO [String]
    getCommandFrom (cmd:stuff) = do
        currentBotState <- asks envBotState >>= liftIO . readMVar
        let
            prefix = botPrefix currentBotState
        if botNickname currentBotState `L.isPrefixOf` cmd 
            then return stuff
            else if isJust prefix 
                   then return (if fromJust prefix `L.isPrefixOf` cmd 
                                  then L.drop (L.length (fromJust prefix)) cmd : stuff 
                                  else cmd:stuff) 
                   else return []
    getCommandFrom _ = return []
getCommandFilter _ = return []

getCommandNoFilter :: IRC.Message -> Env IO [String]
getCommandNoFilter (IRC.Message _ _ [_,msg]) = return $ words $ U.toString msg
getCommandNoFilter _ = return []

-- | Get the sender of a message
getSender :: IRC.Message -> String
getSender (IRC.Message (Just (IRC.NickName nick _ _)) _ _) = U.toString nick
getSender _ = ""

-- | Get the destination of a message
getDestination :: IRC.Message -> String
getDestination (IRC.Message _ _ [dest, _]) = U.toString dest
getDestination _ = ""

