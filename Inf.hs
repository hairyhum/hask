module Inf where

import Data.Time
import Data.Time.Clock

mon :: Int -> IO ()
mon a = putStrLn (show a)

timeFun :: IO Day
timeFun = getCurrentTime >>= \d -> return $ utctDay d

