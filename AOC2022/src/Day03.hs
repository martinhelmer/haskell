module Day03 (run, runtest) where

import AOCHelper


-- import Data.List (sort)
import qualified Data.Set as Set
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Char (ord)

test :: String
test = unlines ["vJrwpWtwJgWrhcsFMMfFFhFp",
       "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"]

runtest :: IO ()
runtest = do 
     part1 test >>= assertInt 157
     part2 test >>= assertInt 70


run :: IO ()
run = do
   putStrLn "--- Day03 ---"
   putStr " Part1: "
   readInp "input03.txt" >>= part1 >>= assertInt 8185
   putStr " Part2: "
   readInp "input03.txt" >>= part2 >>= assertInt 2817

part1 :: String -> IO Int
part1 s = do
    return $ sum . map prioFromString $ lines s  

prioFromString :: String -> Int 
prioFromString s = case  Set.toList $ Set.intersection a b of 
                    [] -> 0
                    [c] -> charPrio c  
                    _ -> undefined
    where (a,b) = halves s

halves :: Ord a => [a] -> (Set.Set a, Set.Set a)
halves s= join bimap Set.fromList $ splitAt (length s `div` 2) s

charPrio :: Char -> Int
charPrio c = if c > 'Z' then 1 + (ord c - ord 'a') else 27 + (ord c - ord 'A')

--- part 3

of3 :: [a] -> [[a]] -> [[a]]
of3 [] [] = [[]]
of3 [] ys = ys 
of3 (x:y:z:xs) ys = of3 xs ([x,y,z]:ys )
of3 _ _ = undefined


oneIntersectPrio :: [String] -> Int 
oneIntersectPrio s =  case Set.toList $ foldr1  Set.intersection  $  map Set.fromList s of
        [] -> 0
        [c] -> charPrio c 
        _ -> undefined
    
part2 :: String -> IO Int
part2 s = do
    return $ sum . map oneIntersectPrio $ of3 ( lines s ) []
