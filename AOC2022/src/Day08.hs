{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day08 (run, runtest) where

import AOCHelper
import Data.List ( transpose, foldl', tails ) 


test :: String
test = unlines ["30373",
                "25512",
                "65332",
                "33549",
                "35390"]

-- first implementation
visRow :: [Char] -> [Char]
visRow = go ' ' []
    where go _ acc [] = reverse acc 
          go m acc (x:xs) | x > m = go x ('1':acc) xs 
                          | otherwise = go m ('0':acc) xs 

visRow' :: String -> String 
visRow' = reverse . snd . foldl' go (' ',[]) 
    where go (m, acc) x | x > m = (x, '1':acc)
                        | otherwise = (m, '0':acc)

scoreRow :: Ord a => [Int] -> [a] -> [Int]
scoreRow acc [] = reverse acc
scoreRow acc (x:xs) = scoreRow (go:acc) xs 
            where  go = let d = dropWhile (<x) xs in   
                            length xs - length d + (fromEnum . not . null $ d)

scoreRow' :: String -> [Int]
scoreRow' = map go . init . tails
    where go (x:xs) = let d = dropWhile (<x) xs in   
                            length xs - length d + (fromEnum . not . null $ d)
          go [] = undefined

bothDir :: (b -> b -> c) -> ([a] -> [b]) -> [a] -> [c]
bothDir f rowf xs = zipWith f (rowf xs) (reverse $ rowf (reverse xs))

matrix :: (b -> b -> c) -> ([a] -> [b]) -> [[a]] -> [[c]]
matrix f rowf  = map (bothDir f rowf)

tot :: (b -> b -> b) -> ([a] -> [b]) -> [[a]] -> [[b]]
tot f rowf xs = zipWith (zipWith f) (m xs) (transpose . m . transpose $ xs)
    where m = matrix f rowf


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 21
     part2 test >>= assertIt 8

run :: IO ()
run = do
   putStrLn "--- Day08 TreeHouse---"
   putStr " Part1: "
   readInp "input08.txt" >>= part1 >>= assertIt 1823
   putStr " Part2: "
   readInp "input08.txt" >>= part2 >>= assertIt 211680

part1 :: String -> IO Int
part1 s = do
    return $ length . filter (== '1') . concat . tot max visRow' . lines $ s  
    
part2 :: String -> IO Int
part2 s = do
    return $ maximum . concat . tot (*) scoreRow' . lines $ s 

