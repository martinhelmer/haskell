{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day10 (run) where

import AOCHelper ( assertIt, readInp, intDispl )
import  Data.List.Split (chunksOf)

scanIt :: [String] -> [Int]
scanIt = concatMap fst . scanl go ([],1) 
    where go (_,w) i | i == "noop" = ([w],w)
                     | otherwise = (replicate 2 w, w +  (read . drop 5 $ i))

fullSum :: String -> Int
fullSum s = sum $ map (\i -> i * (l !! (i-1) )) [20, 60, 100, 140, 180, 220]
                 where l = scanIt $ lines s 

crtOutput :: String -> String 
crtOutput s =zipWith o (concat . repeat $ [0..39]) (scanIt $ lines s)
    where o crt sprite | abs (crt - sprite) < 2 = intDispl (1::Integer)
                       | otherwise = '.'

part1 :: String -> IO Int
part1 s = do
    return $ fullSum s 
    
part2 :: String -> IO String 
part2 s = do
    let answer = unlines $ chunksOf 40 $ crtOutput s 
    putStrLn $ ""
    putStrLn answer
    return $ "RUAKHBEK"

run :: IO ()
run = do
   putStrLn "--- Day10 ---"
   putStr " Part1: "
   readInp  "input10.txt" >>= part1 >>= assertIt 13220
   putStr " Part2: "
   readInp "input10.txt" >>= part2 >>= assertIt "RUAKHBEK"


