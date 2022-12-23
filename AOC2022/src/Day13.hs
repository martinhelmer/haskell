{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Day13 (run) where

import AOCHelper ( assertIt, readInp)
import Data.Char (isDigit)
import Text.RawString.QQ ( r )
import Data.List.Split (splitOn)
import Data.List ( sort ) 

parseInput1 s = map(\[a,b] -> parseTree a < parseTree b) . map words $ ss
     where ss = splitOn "\n\n" s 

part1 :: String -> IO Int
part1 s = do
  return $ sum . map (\(i,b) -> if b then i else 0) $ zip [1..] (parseInput1 s)


part2 :: String -> IO Int
part2 s = do
  let packets = dp1:dp2:(map parseTree . lines $ unlines $ splitOn "\n\n" s)
  return $ product . map (\(i,b) -> if (b==dp1 || b == dp2) then i else 1 )     $ zip [1..] (sort packets)

run :: IO ()
run = do
   putStrLn "--- Day13 List Hell ---"
   putStr " Part1: "
   readInp  "input13.txt" >>= part1 >>= assertIt 5503
   putStr " Part2: "
   readInp "input13.txt" >>= part2 >>= assertIt 20952

dp1 = parseTree "[[2]]"
dp2 = parseTree "[[6]]"

test = [r|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|]

data Tree = Node Int | L [Tree] deriving Show 

instance Eq Tree where 
    Node a == Node b = a == b
    L tl1 == L tl2 =  tl1 == tl2
    Node a == L tl1 = [Node a] == tl1
    L tl1 == Node a = Node a == L tl1 

instance Ord Tree where 
    t1 <= t2 = t1 < t2 || t1 == t2 
    Node a < Node b = a < b 
    L [] < L [] = False 
    L [] < L (x:_) = True 
    L (_:_) < L [] = False 
    L (x:xs) < L (y:ys) = x < y || (x == y && (L xs) < (L ys))
    Node a < L b = L [Node a] < L b
    L b < Node a =  L b < L [Node a]


parseTree (x:xs) = L (fst . pl [] $ xs)

pl :: [Tree] -> String  -> ([Tree], String)
pl elems [] = (elems, "")
pl elems (x:xs) | x == ',' = pl elems xs 
                | x == ']' = (reverse elems, xs)
                | isDigit x = pl (Node (read (takeWhile isDigit (x:xs) )):elems) (dropWhile isDigit xs) 
                | x == '[' = let (e,r) = pl [] xs in pl (L e:elems) r 
                | otherwise = error "Q"



eq :: [Char] -> [Char] -> Bool
eq a@(x:xs) b@(y:ys) | a == b = True 
                     | x == y = eq xs ys 
                     | isDigit x && y == '[' =  eq ('[':takeWhile isDigit a++(']':(dropWhile isDigit a)))  b
                     | otherwise = False 

-- lt a@(x:xs) b@(y:ys) | x == y = lt xs ys 
--                      | x == ']' = True 
--                      | y == ']' = False 
--                      | x = '[' 