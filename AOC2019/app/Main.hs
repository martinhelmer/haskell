module Main where
import System.Environment

import Day01
import Day02
import Day02b
import Day03
import Day04
import Day04_other
import Day05
import Day06
import Day07 
import Day08 
import Day09 
import Day10
import Day11
import Day12
import Day12b
import Day13 
import Day14 
import Day15 
import Day16 
import Day18
import Day18b 
import Day19 
import Day20 

import Control.Monad
import System.TimeIt

sToR :: String -> IO ()
sToR "ALL" = forM_ [timeIt Day01.run, timeIt Day02.run, timeIt Day02b.run,
                    timeIt Day03.run, timeIt Day04.run, timeIt Day04_other.run, 
                    timeIt Day05.run, timeIt Day06.run, timeIt Day07.run,
                    timeIt Day08.run, timeIt Day09.run, timeIt Day10.run,
                    timeIt Day11.run, timeIt Day12b.run, timeIt Day13.run, timeIt Day14.run,
                    timeIt Day15.run, timeIt Day16.run, timeIt Day18.run]  id

sToR "01" = Day01.run
sToR "02" = Day02.run
sToR "02b" = Day02b.run
sToR "03" = Day03.run
sToR "04" = Day04.run
sToR "04b" = Day04_other.run
sToR "05" = Day05.run
sToR "06" = Day06.run
sToR "07" = Day07.run
sToR "08" = Day08.run
sToR "09" = Day09.run
sToR "10" = Day10.run
sToR "11" = Day11.run
sToR "12b" = Day12b.run
sToR "13" = Day13.run
sToR "14" = Day14.run
sToR "15" = Day15.run
sToR "16" = Day16.run
sToR "18" = Day18.run
sToR "18b" = Day18b.run
sToR "19" = Day19.run
sToR "20" = Day20.run




sToR _ = undefined


main :: IO ()
main = do
    args <- getArgs
    timeIt $ sToR $ head args
