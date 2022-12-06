module Day02 (run, runtest) where

import AOCHelper
-- import Data.List.Split
-- import Data.List (sort)

-- A, X = Rock
-- B. Y = Paper
-- C, Z = Scissors

playPoints :: Char -> Int
playPoints 'A' = 1
playPoints 'X' = 1
playPoints 'B' = 2
playPoints 'Y' = 2
playPoints 'C' = 3
playPoints 'Z' = 3
playPoints _ = undefined

winPoints :: Char -> Char -> Int 
winPoints them me = case mp - tp of
                            0 -> 3    -- draw
                            1 -> 6    -- win 
                            -2 -> 6   -- win "C X"
                            _ -> 0 
    where mp = playPoints me 
          tp = playPoints them 

-- assuming s is on the form 'A X'
roundPointsFromString :: [Char] -> Int
roundPointsFromString s = roundPoints (head s) (last s)

roundPoints :: Char -> Char -> Int 
roundPoints them me = playPoints me + winPoints them me

test :: String
test = "A Y\nB X\nC Z"

runtest :: IO ()
runtest = do 
     part1 test >>= assertInt 15
     part2 test >>= assertInt 12


run :: IO ()
run = do
   putStrLn "--- Day02 ---"
   putStr " Part1: "
   readInp "input02.txt" >>= part1 >>= assertInt 10718
   putStr " Part2: "
   readInp "input02.txt" >>= part2 >>= assertInt 14652

part1 :: String -> IO Int
part1 s = do
    return $ sum . map roundPointsFromString $ lines s  

-- part 2 
yourMove :: String -> Char
yourMove "A X" = 'C'
yourMove "A Y" = 'A'
yourMove "A Z" = 'B'
yourMove "B X" = 'A'
yourMove "B Y" = 'B'
yourMove "B Z" = 'C'
yourMove "C X" = 'B'
yourMove "C Y" = 'C'
yourMove "C Z" = 'A'
yourMove _ = undefined


roundPointsFromString2 :: [Char] -> Int
roundPointsFromString2 s = roundPoints (head s) (yourMove s)

part2 :: String -> IO Int
part2 s = do
    return $ sum . map roundPointsFromString2 $ lines s  