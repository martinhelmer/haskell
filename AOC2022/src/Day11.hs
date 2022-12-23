{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}


module Day11 (run) where

import Text.RawString.QQ ( r )
import AOCHelper ( assertIt, readInp )
import  Data.List.Split (splitOn)
import qualified Data.IntMap as M
import Data.List ( foldl', sort )
import Data.Maybe (fromJust)

type Op = Int -> Int 

instance Show Op where 
    show f | f 1 == 1 && f 0 == 0 && f 2 == 4 = "old * old"
           | f 0 == 0 = "old * " ++ show (f 1)
           | otherwise = "old + " ++ show (f 0)

data Monkey = Monkey { name :: Int
                     , count :: Int
                     , items :: [Int]
                     , monkeyop :: Op 
                     , testdiv :: Int
                     , truemonix :: Int
                     , falsemonix :: Int
                     } deriving (Show)

clear :: Int -> Monkey -> Monkey
clear n (Monkey a b _ d e f g) = Monkey a (b+n) [] d e f g  

addThings :: [Int] -> Monkey -> Monkey
addThings things (Monkey n c itms op tdiv tm fm) = Monkey n c (itms ++ things) op tdiv tm fm

parseMonkey :: String -> Monkey
parseMonkey s = Monkey (read . init . last . words $ l1)
                        0
                        (read $ "["++(last . splitOn  ": " $ l2) ++"]")
                        (mop . drop 4 . words $ l3)  
                        (read . last . words $ l4)
                        (read . last . words $ l5)
                        (read . last . words $ l6)

        where [l1, l2, l3, l4, l5, l6 ] = lines s 
              mop ["*", "old"] = \x -> x * x
              mop ["*", y] = \x -> x * read y
              mop ["+", y] = \x -> x + read y

parseMonkeys :: [String] -> M.IntMap Monkey
parseMonkeys xs = M.fromList (map (\s ->let m = parseMonkey s in (name m, m)) xs)

monkeyThrows' :: (Int -> Int)  -> Monkey -> ([Int],[Int])
monkeyThrows' worry monkey =  foldl' go ([],[]) (items monkey)
                    where go (tl,fl) x = let new =  worry (monkeyop monkey x) in 
                                         if new `mod` testdiv monkey == 0 then (new:tl,fl) else (tl,new:fl)

doOne :: (Int -> Int) -> M.IntMap Monkey -> Int -> M.IntMap Monkey
doOne worry m index = M.update (Just . clear (length . items $ thismonkey)) index $
                      M.update (Just . addThings truethings) (truemonix thismonkey) $ 
                      M.update (Just . addThings falsethings) (falsemonix thismonkey) m 
            where
              (truethings, falsethings) = monkeyThrows' worry thismonkey 
              thismonkey = fromJust (M.lookup index m)

round ::  (Int -> Int) -> M.IntMap Monkey -> M.IntMap Monkey
round worry m = foldl' (doOne worry)  m (M.keys m)

counts :: M.IntMap Monkey -> [Int]
counts m = map count $ M.elems m

monkeyBusiness :: (Int -> Int) -> Int -> M.IntMap Monkey -> Int
monkeyBusiness worry rounds m = product . take 2 . reverse . sort . counts $ iterate (Day11.round worry) m !! rounds

part1 :: String -> IO Int
part1 s = do
    let m = parseMonkeys $  splitOn "\n\n" s
    return $ monkeyBusiness (`div` 3) 20 m

part2 :: String -> IO Int
part2 s = do
    let m = parseMonkeys $  splitOn "\n\n" s
    return $ monkeyBusiness (`mod` (11 * 7 * 3 * 5 * 17 * 13 *19 * 2)) 10000 m
    

test :: String 
test = [r|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|]


mt = parseMonkey test 

run :: IO ()
run = do
   putStrLn "--- Day11 Monkey Business ---"
   putStr " Part1: "
   readInp  "input11.txt" >>= part1 >>= assertIt 102399
   putStr " Part2: "
   readInp "input11.txt" >>= part2 >>= assertIt 23641658401


