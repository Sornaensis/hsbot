module Hsbot.Plugin.Scheme
    ( hsch
    , thehsch
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC
import qualified Data.List as L

import Hsbot.Message hiding (getCommand)
import Hsbot.Types
import Hsbot.Utils

import qualified Data.ByteString.UTF8 as U

import Text.ParserCombinators.Parsec hiding (spaces)

-- | The hsch plugin identity
hsch :: PluginId
hsch = PluginId
    { pluginName = "hsch"
    , pluginEp   = thehsch 
    , pluginCmds = Just theCmds }

theCmds :: PluginCmds
theCmds = [ 
            PluginCmd
            { command     = ">>"
            , cmdIsPrefix = True
            , cmdFunc     = doEval
            , cmdPerm     = Just Admin
            , cmdHelp     = "Script REPL"
            }
          ]

getCommand :: IRC.Message -> Env IO [String]
getCommand = getCommandNoFilter

-- | An IRC plugin for hsching text
thehsch :: Plugin (Env IO) ()
thehsch = forever $ readMsg >>= eval 
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg) 
        | IRC.msg_command msg == U.fromString "PRIVMSG" =
                   lift (getCommand msg) >>= 
                   \cmdArgs -> void $ mapM_ (parseCommand msg cmdArgs) theCmds 
        | otherwise = return ()
    eval _ = return ()

doEval :: IRC.Message -> [String] -> Plugin (Env IO) ()
doEval msg xs = case readExpr (unwords xs) of
                    Just str -> answerMsg msg str
                    _        -> return ()

data ExprVal = String String 
             | Number Integer
             | Param  String
             | Atom   String
             | List   [ExprVal]
             | EList  [ExprVal]

instance Show ExprVal where show = showExpr

showExpr :: ExprVal -> String
showExpr (String s) = "\"" ++ s ++ "\""
showExpr (Param n)  = "%" ++ show n
showExpr (Atom a)   = a
showExpr (Number n) = show n
showExpr (List l)   = "[" ++ L.intercalate ", " (unwordsExpr l) ++ "]"
showExpr (EList l)  = "{" ++ unwords (unwordsExpr l) ++ "}"

unwordsExpr :: [ExprVal] -> [String]
unwordsExpr = map showExpr 

spaces :: Parser ()
spaces = skipMany1 space

parseStringChar :: Parser Char
parseStringChar = do
                  echar  <- noneOf "\""
                  case  echar  of
                      '\\' -> oneOf "\'\"\\"
                      _    -> return echar

parseParam :: Parser ExprVal
parseParam = do
             char '%'
             n <- many1 (digit <|> letter <|> symbol)
             return $ Param n

parseAtom :: Parser ExprVal
parseAtom = do
            first <- letter <|> char '_'
            rest <- many (letter <|> digit <|> symbol)
            return $ Atom (first:rest)
                                 
parseString :: Parser ExprVal
parseString = do
              char '"' 
              x <- many parseStringChar
              char '"'
              return $ String x

parseList :: Parser ExprVal
parseList = do
            char '['
            skipMany space
            list <- liftM List $ sepBy (skipMany space >> (parseList  <|> parseString <|> 
                                                            parseAtom <|> parseNumber <|> 
                                                            parseParam)) (try $ skipMany space >> char ',')
            skipMany space
            char ']'
            return list

parseNumber :: Parser ExprVal
parseNumber = do
              sign <- char '-' <|> digit 
              num <- many1 digit 
              return $ case reads (sign:num) of
                           [(n, "")] -> Number n 

symbol :: Parser Char
symbol = oneOf "_:-"

parseBraces :: Parser ExprVal
parseBraces = do   
              char '{'
              skipMany space
              expr <- manyTill (skipMany space >> (parseString  <|> parseAtom   <|> 
                                                    parseList   <|> parseBraces <|>
                                                    parseNumber <|> parseParam)) 
                                                  (try $ skipMany space >> char '}')
              return $ EList expr

parseExprs :: Parser [ExprVal]
parseExprs = many (skipMany space >> parseBraces)

evalExpr :: ExprVal -> String
evalExpr (String str) = str
evalExpr (Param name) = "%" ++ name
evalExpr (EList xs)   = last $ map evalExpr xs
evalExpr (List xs)    = last $ map evalExpr xs

readExpr :: String -> Maybe String
readExpr input = case parse parseExprs "hsbot2::eval" input of
                    Left err  -> Just . unwords . lines $ show err
                    Right val -> case last (map evalExpr val) of
                                        []  -> Nothing
                                        str -> Just str
