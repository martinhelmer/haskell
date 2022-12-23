{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 (run, runtest) where

import AOCHelper ( assertIt, readInpByteSTring )
import Data.Containers.ListUtils ( nubOrd )
import Data.ByteString ( ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BSU 
import Data.Maybe ( fromJust ) 

moves :: [ByteString] -> [(Int,Int)]
moves = concatMap  go 
    where go line = replicate (fst $ fromJust $ BC.readInt (BC.drop 2 line)) (md line)
          md c = case BC.head c of 
                'R' -> (1,0) 
                'L' -> (-1,0) 
                'U' -> (0,-1) 
                'D' -> (0,1) 
                _ -> undefined

hPos :: [ByteString] -> [(Int, Int)]
hPos s = scanl (\(a,b) (c,d) -> (a+c,b+d)) (0,0) $ moves s  

tPos :: [(Int, Int)] -> [(Int, Int)]
tPos = scanl1 go  
    where go (tx,ty) (hx,hy) = if isFar then ( d tx hx , d ty hy )
                                        else (tx,ty)
            where
                d a b = a + signum (b-a)
                isFar = any ((1 <) . abs) [hx-tx, hy-ty]

tailVisits :: Int -> ByteString -> Int
tailVisits n s = length . nubOrd $ iterate tPos (hPos (init $ BC.split '\n' s)) !! n

part1 :: ByteString -> IO Int
part1 s = do
    return $ tailVisits 1 s
    
part2 :: ByteString -> IO Int
part2 s = do
    return $ tailVisits 9 s 
        
test :: ByteString
test = BSU.fromString . 
       unlines $ ["R 4",
                "U 4",
                "L 3",
                "D 1",
                "R 4",
                "D 1",
                "L 5",
                "R 2"]

test2 :: ByteString
test2 = BSU.fromString . 
        unlines $ ["R 5",
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
   readInpByteSTring  "input09.txt" >>= part1 >>= assertIt 6494
   putStr " Part2: "
   readInpByteSTring "input09.txt" >>= part2 >>= assertIt 2691


