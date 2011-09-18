{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that manages quotes for posterity and legend
module Hsbot.Plugin.Quote
    ( quote
    , theQuote
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.Map as M
import Data.Maybe
import Data.SafeCopy
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import qualified Network.IRC as IRC
import System.Environment.XDG.BaseDir
import System.Random

import Hsbot.Message
import Hsbot.Types

-- | A quote element
data QuoteElt = QuoteElt
    { eltQuotee :: IRC.UserName
    , eltQuote  :: String
    } deriving (Show, Typeable)

type QuoteID = Int

-- | A quote object
data Quote = Quote
    { quoter    :: IRC.UserName
    , quotE     :: [QuoteElt]
    , quoteTime :: UTCTime
    , votes     :: Int
    , voters    :: M.Map IRC.UserName QuoteID
    } deriving (Show, Typeable)

emptyQuote :: Quote
emptyQuote = Quote { quoter = ""
                   , quotE = []
                   , quoteTime = posixSecondsToUTCTime 0
                   , votes = 0
                   , voters = M.empty }

-- The Quote database
data QuoteDB = QuoteDB
    { nextQuoteId  :: QuoteID
    , quoteBotDB   :: M.Map QuoteID Quote
    , lockedQuotes :: M.Map QuoteID (IRC.UserName, UTCTime)
    , lastActive   :: M.Map IRC.Channel QuoteID
    } deriving (Show, Typeable)

emptyQuoteDB :: QuoteDB
emptyQuoteDB = QuoteDB { nextQuoteId  = 0
                       , quoteBotDB   = M.empty
                       , lockedQuotes = M.empty
                       , lastActive   = M.empty }

$(deriveSafeCopy 0 'base ''QuoteElt)
$(deriveSafeCopy 0 'base ''Quote)
$(deriveSafeCopy 0 'base ''QuoteDB)

-- | Quote database transactions
getQuote :: QuoteID -> Query QuoteDB (Maybe Quote)
getQuote quoteId = fmap (M.lookup quoteId) (asks quoteBotDB)

getQuoteDB :: Query QuoteDB (M.Map QuoteID Quote)
getQuoteDB = asks quoteBotDB

-- TODO : a function for cleaning locks

isQuoteLockedFor :: QuoteID -> IRC.UserName -> UTCTime -> Query QuoteDB (Maybe Bool)
isQuoteLockedFor quoteId requestor now = do
    theQuote <- fmap (M.lookup quoteId) (asks quoteBotDB)
    case theQuote of
        Just _ -> do
            currentLock <- fmap (M.lookup quoteId) (asks lockedQuotes)
            case currentLock of
                Just (owner, lockStamp) ->
                  if owner == requestor
                    then return $ Just True
                    else return . Just $ (addUTCTime 300 lockStamp > now)     -- Is the entry older than 5 min?
                Nothing -> return $ Just True
        Nothing -> return Nothing

lockQuoteIdFor :: QuoteID -> IRC.UserName -> UTCTime -> Update QuoteDB ()
lockQuoteIdFor quoteId requestor now = get >>= \db -> put db { lockedQuotes = M.insert quoteId (requestor, now) (lockedQuotes db) }

setQuote :: QuoteID -> Quote -> Update QuoteDB ()
setQuote quoteId theQuote = get >>= \db -> put db { quoteBotDB = M.insert quoteId theQuote (quoteBotDB db) }

getLastActiveQuote :: IRC.Channel -> Query QuoteDB (Maybe QuoteID)
getLastActiveQuote channel = asks lastActive >>= return . M.lookup channel

setLastActiveQuote :: QuoteID -> IRC.Channel -> Update QuoteDB ()
setLastActiveQuote quoteId channel = get >>= \db -> put db { lastActive = M.insert channel quoteId (lastActive db)}

$(makeAcidic ''QuoteDB [ 'getQuote, 'getQuoteDB, 'isQuoteLockedFor, 'lockQuoteIdFor, 'setQuote
                       , 'getLastActiveQuote, 'setLastActiveQuote ])

-- | gets a random quote from the database
getRandomQuote :: AcidState QuoteDB -> IO (Maybe Quote)
getRandomQuote quoteDB = do
    db <- query' quoteDB GetQuoteDB
    if M.size db > 0
      then getStdRandom (randomR (0, M.size db - 1)) >>= \rInt -> return . Just . snd $ M.elemAt rInt db
      else return Nothing

-- | The duck plugin identity
quote :: PluginId
quote = PluginId
    { pluginName = "quote"
    , pluginEp   = theQuote }

-- | An IRC plugin that generates and kills ducks
theQuote :: Plugin (Env IO) ()  -- TODO : an argument for the history size
theQuote = do
    baseDir <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot"
    quoteDB <- liftIO $ openAcidStateFrom (baseDir ++ "/quoteDB/") emptyQuoteDB
    forever $ readMsg >>= eval quoteDB
  where
    eval :: AcidState QuoteDB -> Message -> Plugin (Env IO) ()
    eval quoteDB (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                "quote":"append":quoteID:quotee:quoteTxt ->
                    case reads quoteID :: [(Int, String)] of
                        (qid,_):_ -> quoteAppend quoteDB msg qid quotee $ unwords quoteTxt
                        _ -> do
                            lastQid <- query' quoteDB (GetLastActiveQuote (getChannel msg))
                            case lastQid of
                                Just qid -> quoteAppend quoteDB msg qid quotee . unwords $ quoteID : quoteTxt
                                Nothing -> answerMsg msg $ getSender msg ++ " : Invalid quoteID."
                "quote":"help":"append":_ -> answerMsg msg $ "quote append [QUOTEID] QUOTEE QUOTE"
                                             ++ "If no QUOTEID is provided, tries to append to the last active quote."
                "quote":"help":"delete":_ -> do
                    answerMsg msg "quote delete QUOTEID [ELTID] :"
                    answerMsg msg $ "  If an ELTID is provided, deletes the ELTID's line (starting from zero) "
                                 ++ "in the quote QUOTEID. If not the whole quote is deleted."
                "quote":"help":"quick":_ -> do
                    answerMsg msg "quote [quick] QUOTEE [QUOTE] :"
                    answerMsg msg $ "  Begins a quote for QUOTEE. You must provide the keywork quick if the "
                                 ++ "QUOTEE's nickname is a reserved word for this quote module. If no QUOTE is "
                                 ++ "provided this module lookup it's conversation history and records the "
                                 ++ "last sentence of QUOTEE."
                "quote":"help":"show":_ -> answerMsg msg "quote show { QUOTEID | random [MIN_SCORE] }"
                "quote":"help":"stat":_ -> do
                    answerMsg msg "quote stat"
                    answerMsg msg "  Compute statistics about the quote database : Most quoters, most quoted "
                "quote":"help":[] ->
                    answerMsg msg $ "Usage: quote { [quick] QUOTEE [QUOTE] | append [QUOTEID] QUOTEE QUOTE | "
                                 ++ "delete QUOTEID [ELTID] | show { QUOTEID | random [MIN_SCORE] } | stat }"
                "quote":"help":_ -> answerMsg msg "Invalid help topic."
                "quote":_ -> answerMsg msg "Invalid quote command."
                "vote":"help":"quick":_ -> do
                    answerMsg msg "vote [quick] [QUOTEID] { +1 | -1 | ++ | -- }"
                    answerMsg msg $ "  Vote for a quote. You can also vote for the last active quote on this chan "
                                 ++ "by typing something that begins by +1, -1 or ++ or --."
                "vote":"help":[] -> answerMsg msg "Usage: vote { [quick] [QUOTEID] { +1 | -1 } | show [QUOTEID] | stat }"
                "vote":"help":_ -> answerMsg msg "Invalid help topic."
                "vote":_ -> answerMsg msg "Invalid vote command."
                _ -> return ()
        | otherwise = return ()
    eval _ _ = return ()

quoteAppend :: AcidState QuoteDB -> IRC.Message -> QuoteID -> IRC.UserName -> String -> Plugin (Env IO) ()
quoteAppend quoteDB msg quoteID quotee text = do
    now <- liftIO getCurrentTime
    activeLock <- query' quoteDB (IsQuoteLockedFor quoteID sender now)
    case activeLock of
        Just True -> do
            _ <- update' quoteDB (LockQuoteIdFor quoteID sender now)
            mQuote <- query' quoteDB (GetQuote quoteID)
            let newQuote = fromMaybe emptyQuote mQuote
                newQuote' = newQuote { quotE = quotE newQuote ++ [ QuoteElt { eltQuotee = quotee, eltQuote = text } ] }
            _ <- update' quoteDB (SetQuote quoteID newQuote')
            _ <- update' quoteDB (SetLastActiveQuote quoteID (getChannel msg))
            answerMsg msg $ sender ++ ": Appended to quote " ++ show quoteID
        Just False -> answerMsg msg $ sender ++ ": Someone else is editing this quote right now."
        Nothing -> answerMsg msg $ sender ++ ":quoteId not found."
  where
    sender = getSender msg

