import Hsbot
import Hsbot.Config

import System.Environment 

main :: IO ()
main = getArgs >>= \xs -> case xs of
                              (x:_) -> hsbot x defaultConfig
                              _     -> hsbot' defaultConfig 

