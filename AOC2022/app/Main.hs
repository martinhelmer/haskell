module Main (main) where
import System.Environment

import Day01
import Day02 
import Day03 
import Day04
import Day05
import Day06
import Day07
import Day07b 
import Day08 
import Day09 
import Day10 
import Day11 
import Day12 
import Day13
import Day14 
import Day15 
import Day16 
import Day17
import Day18 
import Day19 
import Day20 
import Day20b
import Day22
import Day23 
import Day24 

import Control.Monad
import System.TimeIt

sToR :: String -> IO ()
sToR "ALL" = forM_ [timeIt Day01.run, timeIt Day02.run, timeIt Day03.run,
                    timeIt Day04.run, timeIt Day05.run, timeIt Day06.run,
                    timeIt Day07.run, timeIt Day07b.run, timeIt Day08.run,
                    timeIt Day09.run, timeIt Day10.run, timeIt Day11.run,
                    timeIt Day12.run, timeIt Day13.run, timeIt Day14.run, 
                    timeIt Day15.run, timeIt Day16.run, timeIt Day17.run, 
                    timeIt Day18.run,  timeIt Day19.run, timeIt Day20b.run]  id
sToR "01" = Day01.run
sToR "02" = Day02.run
sToR "03" = Day03.run
sToR "04" = Day04.run
sToR "05" = Day05.run
sToR "06" = Day06.run
sToR "07" = Day07.run
sToR "07b" = Day07b.run
sToR "08" = Day08.run
sToR "09" = Day09.run
sToR "10" = Day10.run
sToR "11" = Day11.run
sToR "12" = Day12.run
sToR "13" = Day13.run
sToR "14" = Day14.run
sToR "15" = Day15.run
sToR "16" = Day16.run
sToR "17" = Day17.run
sToR "18" = Day18.run
sToR "19" = Day19.run
sToR "20" = Day20.run
sToR "22" = Day22.run
sToR "23" = Day23.run
sToR "24" = Day24.run

sToR _ = undefined

main :: IO ()
main = do
    args <- getArgs
    timeIt $ sToR $ head args
