{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}

module Day01 (run) where

import AOCHelper
import Data.List.Split
-- import Data.List 

-- test :: String
-- test = "888\n333\n\n666\n"

--  for part 2 
calorieList :: (Num b, Read b) => String -> [b]
calorieList s =   map (sum . map read) $ splitOn [""] ( lines s )

-- alternative, equally fast it seems
-- calorieList' :: [Int] -> [String] -> [Int]
-- calorieList' xs [] = xs
-- calorieList' xs ("":ys) = calorieList' (0:xs) ys
-- calorieList' (x:xs) (y:ys) = calorieList' ((x+read y):xs) ys 


maxof3 :: [Int] -> Int
maxof3 = maxof3' 0 0 0 
   where 
    maxof3' a b c [] = a + b + c 
    maxof3' a b c (x:xs) | x > a = maxof3' x a b xs
                        | x > b = maxof3' a x b xs 
                        | x > c = maxof3' a b x xs
                        | otherwise = maxof3' a b c xs 
run :: IO ()
run = do
   putStrLn "--- Day01 counting calories ---"
   putStr " Part1: "
   readInp "input01.txt" >>= part1 >>= assertInt 69177
   putStr " Part2: "
   readInp "input01.txt" >>= part2 >>= assertInt 207456

part1 :: String -> IO Int
part1 s = do
    return $ maximum . calorieList $ s


part2 :: String -> IO Int
part2 s = do
    return $ maxof3 . calorieList $ s 
    -- return $ sum . take 3 . reverse . sort . calorieList $ s



