{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 (run, runtest) where

import AOCHelper ( assertIt, readInp )
import Data.Containers.ListUtils ( nubOrd )

moves :: [String] -> [(Int,Int)]
moves = concatMap  go 
    where go (d:_:n) = replicate (read n) (md d)
          md c = case c of 
                'R' -> (1,0) 
                'L' -> (-1,0) 
                'U' -> (0,-1) 
                'D' -> (0,1) 
                _ -> undefined

hPositions :: [String] -> [(Int, Int)]
hPositions s = scanl (\(a,b) (c,d) -> (a+c,b+d)) (0,0) $ moves s  

tPositions :: [(Int, Int)] -> [(Int, Int)]
tPositions = scanl1 go  
    where go (x,y) (x2,y2) = ( d x x2 , d y y2 )
             where
                d a b | b > a && far = a+1
                      | b < a && far = a-1 
                      | otherwise = a 
                far = abs (x2-x) > 1 || abs (y2-y) > 1


tailN n s =  iterate tPositions (hPositions $ lines s) !! n

part1 :: String -> IO Int
part1 s = do
    return $ length . nubOrd $ tailN 1 s
    
part2 :: String -> IO Int
part2 s = do
    return $ length . nubOrd $ tailN 9 s
    
test :: String
test = unlines ["R 4",
                "U 4",
                "L 3",
                "D 1",
                "R 4",
                "D 1",
                "L 5",
                "R 2"]

test2 = unlines ["R 5",
                "U 8",
                "L 8",
                "D 3",
                "R 17",
                "D 10",
                "L 25",
                "U 20"]

runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 13
     part2 test >>= assertIt 1
     part2 test2 >>= assertIt 36


run :: IO ()
run = do
   putStrLn "--- Day09 Knotted Rope---"
   putStr " Part1: "
   readInp "input09.txt" >>= part1 >>= assertIt 6494
   putStr " Part2: "
   readInp "input09.txt" >>= part2 >>= assertIt 2691


