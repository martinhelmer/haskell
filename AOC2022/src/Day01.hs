module Day01 (run) where

import AOCHelper
import Data.List.Split
import Data.List (sort)

test = "888\n333\n\n666\n"

--  for part 2 
calorieList :: (Num b, Read b) => String -> [b]
calorieList s =   map (\x -> sum $  map read x) $ splitOn [""] ( lines s )

run :: IO ()
run = do
   putStrLn "--- Day01 ---"
   putStr " Part1: "
   readInp "input01.txt" >>= part1 >>= assertInt 69177
   putStr " Part2: "
   readInp "input01.txt" >>= part2 >>= assertInt 207456

part1 :: String -> IO (Int)
part1 s = do
    return $ maximum (calorieList s)


part2 :: String -> IO (Int)
part2 s = do
    return $ sum . take 3 . reverse . sort $ calorieList s



