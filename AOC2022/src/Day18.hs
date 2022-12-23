{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day18 (run) where

import System.TimeIt
import Text.ParserCombinators.ReadP
import AOCHelper ( assertIt, readInp, draw2dset)
import Data.Char (isDigit)
import Text.RawString.QQ ( r )
import  Data.List.Split (splitOn)
import qualified Data.Map as M 
import qualified Data.Set as S
import Control.Applicative
import Data.Maybe ( fromJust )
import GHC.Float (sinDouble)
import Data.List (subsequences, (\\) , sort, transpose)
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))

run :: IO ()
run = do
   s <- readInp  "input18.txt"
   putStrLn "--- Day18 Cubes ---"
   putStr " Part1: "
   timeIt $ part1 s>>= assertIt 4512
   putStr " Part2: "
   timeIt $ part2 s >>= assertIt 2554

test1  = "2,2,2\n1,2,2\n0,2,2\n"

pTest = parseCubes test 

test :: String
test = [r|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5|]


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 64
     part2 test >>= assertIt 58

-----  PARSE
trpl (a,b,c) (d,e,f) = (a+d,b+e,c+f)

type P = (Int,Int,Int)

parseCubes :: String -> S.Set (Int,Int,Int)
parseCubes  = S.fromList . map(\cs -> read ('(':cs++")")) . lines 

cardinals :: P -> [P]
cardinals p = map (trpl p ) $ [(0,0,1), (0,0,-1), (0,1,0), (0,-1,0), (1,0,0), (-1,0,0)]

cn :: S.Set P -> P -> Int
cn cubes p = length . filter (`S.member` cubes ) $ (cardinals p )

--                part 2 
ttl (a,b,c) = [a,b,c]

cagebounds c = ((minimum t0 - 1, minimum t1 -1,minimum t2 - 1),(maximum t0 + 1, maximum t1 + 1, maximum t2 + 1))
    where t' = transpose $ map ttl (S.toList c)
          t0 = t' !! 0
          t1 = t' !! 1
          t2 = t' !! 2

nf :: ((Int, Int, Int), (Int, Int, Int)) -> S.Set P -> P -> [P]
nf ((xmin,ymin,zmin),(xmax,ymax,zmax)) cubes p = filter (\c -> inRange c && not (S.member c cubes))  (cardinals p)
        where inRange (x,y,z) = x>=xmin && x<=xmax && y>=ymin && y <= ymax && z >= zmin && z <= zmax
              

bfs :: (S.Set P) -> (P -> [P]) -> M.Map P Int -> Seq P -> (Maybe P,  M.Map P Int) 
bfs cubes nf' visited queue | Seq.empty == queue = (Nothing, visited)
                        | otherwise = bfs cubes nf' (M.union visited (M.fromList (map (\p -> (p,cn cubes p)) neighbors))) ((><) (Seq.fromList neighbors) popped)
                       where (popped Seq.:|> node) = queue 
                             neighbors = filter (\a -> not $ M.member a visited) (nf' node) 

--- 

part1 ::  String -> IO Int
part1 s = do 
  let cubes = parseCubes s  
  -- print $ S.size cubes
  return $  (6 * (S.size cubes)) - (sum $ map (cn cubes) (S.toList cubes) )

part2 ::  String -> IO Int
part2 s = do  
  let cubes = parseCubes s 
  -- print $ S.size cubes 
  -- print $ cagebounds cubes 
  let (_, visited) = bfs cubes (nf (cagebounds cubes) cubes) (M.singleton (0,0,0) 0)  (Seq.singleton (0,0,0))
  -- print visited 
  return $ sum (M.elems visited)