module Main (main) where
import System.Environment

import Day01
import Day02 

import Control.Monad
import System.TimeIt

sToR :: String -> IO ()
sToR "ALL" = forM_ [timeIt Day01.run, timeIt Day02.run]  id
sToR "01" = Day01.run
sToR "02" = Day02.run
sToR _ = undefined


main :: IO ()
main = do
    args <- getArgs
    timeIt $ sToR $ head args
