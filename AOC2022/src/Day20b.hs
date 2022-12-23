{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Day20b (run) where

import AOCHelper ( readInp, 
                   assertIt)
import           System.TimeIt
import           Data.Int
import           Data.Array.Unboxed
import           Data.Array.ST
import           Control.Monad
import           Data.Vector.Generic (Vector(basicLength))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed         as V
import Data.List ( sort, findIndex, elemIndex, nub ) 
import Data.Maybe ( isNothing, fromJust , isJust ) 

type TheV = V.Vector (Int,Int)


doTwo :: TheV -> Int -> TheV
doTwo v n = if insertpoint == 0 then v else insertAt insertpoint cycled
  where point = V.findIndex (\v -> fst v == n) v 
        point' = fromJust point
        cycled = if point' == 0 then v else let (a,b) = V.splitAt point' v in b V.++ a
        i = (snd . V.head $ cycled) `mod` (V.length cycled - 1)
        insertpoint = if i >= 0 then i else V.length v + i - 1
        insertAt p l =  let (a,b) = V.splitAt p (V.tail l) in a V.++ V.cons (V.head l) b


tv :: V.Vector (Int, Bool)
tv = V.fromList . map (\x -> (read x ::Int,False)) . lines $ test 

part1 ::  String -> IO Int
part1 s = do 
  let origv = V.indexed . V.fromList . map (\x -> ( read x ::Int)) . lines $ s  
  let finished = map snd . V.toList $ iterate (\v -> foldl doTwo v [0..(V.length origv -1)] ) origv !! 1
      zeroAt = fromJust (elemIndex 0 $ finished)
  return  $ sum . map (\i -> (cycle finished) !! (i+zeroAt)) $ [1000, 2000, 3000]

part2 ::  String -> IO Int
part2 s = do 
  let origv = V.indexed . V.fromList . map (\x -> ( 811589153 * read x ::Int)) . lines $ s  
  let finished = map snd . V.toList $ iterate (\v -> foldl doTwo v [0..(V.length origv -1)]) origv !! 10
      zeroAt = fromJust (elemIndex 0 $ finished)
  return  $ sum . map (\i -> (cycle finished) !! (i+zeroAt)) $ [1000, 2000, 3000]

run :: IO ()
run = do
   putStrLn "--- Day20 Circular moving of numbers Vectors ---"
   putStr " Part1: "
   readInp  "input20.txt" >>= part1 >>= assertIt 14526 
   putStr " Part2: "
   readInp  "input20.txt" >>= part2 >>= assertIt 9738258246847


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 3
     part2 test >>= assertIt 1623178306


test = "1\n2\n-3\n3\n-2\n0\n4\n"