module Day06 (run, runtest) where

import AOCHelper
import qualified Data.Set as S
import GHC.Arr (done)
-- import Data.List.Split (splitOn)

test1 :: String
test1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
test2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
test3 = "nppdvjthqldpwncqszvftbrmjlhg"
test4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
test5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"


runtest :: IO ()
runtest = do 
     part1 test1 >>= assertIt 7
     part1 test2 >>= assertIt 5
     part1 test3 >>= assertIt 6
     part1 test4 >>= assertIt 10
     part1 test5 >>= assertIt 11
     part2 test1 >>= assertIt 19
     part2 test2 >>= assertIt 23


run :: IO ()
run = do
   putStrLn "--- Day06 ---"
   putStr " Part1: "
   readInp "input06.txt" >>= part1 >>= assertInt 1275
   putStr " Part2: "
   readInp "input06.txt" >>= part2 >>= assertInt 3605

part1 :: String -> IO Int
part1 s = do
    -- putStrLn s
    return $ length $ process' 4 (reverse $ take 4 s) (drop 4 s)
    
part2 :: String -> IO Int
part2 s = do
    return $ length $ process' 14 (reverse $ take 14 s) (drop 14 s)



process' n done (xs) | areUnique (take n done) = done
                     | null xs = undefined
                     | otherwise = process' n ((head xs):done) (tail xs)

areUnique :: Ord a => [a] -> Bool
areUnique s = length s == length (S.fromList s)