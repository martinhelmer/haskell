module Day04 (run, runtest) where

import AOCHelper
import Data.List.Split (splitOn)

test :: String
test = unlines ["2-4,6-8",
                "2-3,4-5",
                "5-7,7-9",
                "2-8,3-7",
                "6-6,4-6",
                "2-6,4-8"]

runtest :: IO ()
runtest = do 
     part1 test >>= assertInt 2
     part2 test >>= assertInt 4


run :: IO ()
run = do
   putStrLn "--- Day04 ---"
   putStr " Part1: "
   readInp "input04.txt" >>= part1 >>= assertInt 450
   putStr " Part2: "
   readInp "input04.txt" >>= part2 >>= assertInt 837

part1 :: String -> IO Int
part1 s = do
    return $ countCases isFullOverlap s 
    
part2 :: String -> IO Int
part2 s = do
    return $ countCases isPartialOverlap s 

countCases :: ([[Int]] -> Bool) -> String -> Int
countCases f s = length . filter (f . parseLine) $ lines s 

parseLine :: [Char] -> [[Int]]
parseLine s = map (map read . splitOn "-" ) $ splitOn "," s

isFullOverlap :: [[Int]] -> Bool
isFullOverlap [[a,b],[c,d]] = (a <= c && b >= d) || (a >=c && b <= d)
isFullOverlap _ = undefined

isPartialOverlap :: [[Int]] -> Bool
isPartialOverlap [[a,b],[c,d]] = (a <= d) && ( b >= c)
isPartialOverlap _ = undefined