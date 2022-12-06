module Main (main) where
import System.Environment

import Day01
import Day02 
import Day03 
import Day04
import Day05
import Day06

import Control.Monad
import System.TimeIt

sToR :: String -> IO ()
sToR "ALL" = forM_ [timeIt Day01.run, timeIt Day02.run, timeIt Day03.run,
                    timeIt Day04.run, timeIt Day05.run, timeIt Day06.run]  id
sToR "01" = Day01.run
sToR "02" = Day02.run
sToR "03" = Day03.run
sToR "04" = Day04.run
sToR "05" = Day05.run
sToR "06" = Day06.run
sToR _ = undefined


main :: IO ()
main = do
    args <- getArgs
    timeIt $ sToR $ head args
