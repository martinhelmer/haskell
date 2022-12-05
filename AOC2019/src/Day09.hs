module Day09 where

import AOCHelper
import qualified Data.Vector.Unboxed as V
import IntCode

t1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
run :: IO ()
run = do
   putStrLn "Day09 ..."
   putStr " Part1: " >> readInp "input09.txt" >>= part1 >>= assertInt 2351176124
   putStr " Part2: " >> readInp "input09.txt" >>= part2 >>= assertInt 73110

part1 :: String -> IO Int
part1 s = do
    return . head . output $ runProg (parseInp s) [1]

part2 :: String -> IO Int
part2 s = return . head . output $ runProg (parseInp s) [2]


--
parseInp :: String -> V.Vector Int
parseInp s = V.fromList (read $ "[" ++ s ++ "]")
