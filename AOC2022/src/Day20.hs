{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day20 (run) where

import AOCHelper ( readInp, 
                   assertIt)

import qualified Data.Map as M
import Data.List ( sort, findIndex, elemIndex, nub ) 
import Data.Maybe ( isNothing, fromJust , isJust ) 


type NodeKey = Int
data Node = Node { nodemove::Int,
                   prevnode::NodeKey,
                   nextnode:: NodeKey} deriving (Show)
type NodeMap = M.Map Int Node 


ml m k = fromJust $ M.lookup k m

nodestoList :: M.Map NodeKey Node -> [Int]
nodestoList m = map nodemove
                . take (M.size m) 
                . iterate (\n -> ml m (nextnode n))  
                $  ml m 0         

move m ix = if d == 0 then m else insertafter popped targetNix ix  
      where d = (nodemove thisN + M.size m -1) `mod` (M.size m -1)
            thisN = ml m ix 
            targetNix = nextnode $ iterate (ml m . nextnode) thisN !! (d -1)
            popped = pop m ix 

insertafter m ix insertix = foldl(\m (k,a) -> M.insert k a m) m
                            [(ix, Node (nodemove thisN) (prevnode thisN) insertix),
                             (nextNix, Node (nodemove nextN) insertix (nextnode nextN)),
                             (insertix, Node (nodemove insertN) ix nextNix)]
            where thisN = ml m ix 
                  nextNix = nextnode thisN
                  nextN = ml m nextNix 
                  insertN = ml m insertix 
        

pop m ix = foldl(\m (k,a) -> M.insert k a m) m updlist
      where prevNix = prevnode $ ml m ix
            nextNix = nextnode $ ml m ix 
            prevN = ml m prevNix 
            nextN = ml m nextNix
            updlist = [(prevNix,Node (nodemove prevN) (prevnode prevN) nextNix),
                      (nextNix,Node (nodemove nextN) prevNix (nextnode nextN)) ]

parseInp :: Int -> String -> NodeMap
parseInp n s = M.fromList $ zipWith (\i d -> (i,Node d  ((i-1+l) `mod` l) ((i+1+l) `mod` l)))
                           [0..]  ints 
              where ints = map (\x -> n * read x::Int) $ lines s 
                    l = length ints

part1 ::  String -> IO Int
part1 s = do 
  let m = parseInp 1 s  
      final = nodestoList $ foldl move m  [0..(M.size m -1)]
      zeroAt =  fromJust $ elemIndex 0 $ final
  return  (sum . map (\i -> cycle final !! (i+zeroAt)) $ [1000, 2000, 3000])



-- 5655964807257 WRONG 
-- -3502007195195
-- -14340780333510 WRONG 
-- 811589153  
-- -3502007195195 WRONG
part2 ::  String -> IO Int
part2 s = do 
  let m = parseInp 811589153 s  
      final = nodestoList $  iterate (\m' -> foldl move m'  [0..(M.size m -1)]) m !! 10 
      zeroAt =  fromJust $ elemIndex 0 $ final
  return  (sum . map (\i -> cycle final !! (i+zeroAt)) $ [1000, 2000, 3000])

run :: IO ()
run = do
   putStrLn "--- Day20 ---"
   putStr " Part1: "
   readInp  "input20.txt" >>= part1 >>= assertIt 14526 
   putStr " Part2: "
   readInp  "input20.txt" >>= part2 >>= assertIt 9738258246847


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 3
     part2 test >>= assertIt 1623178306


test = "1\n2\n-3\n3\n-2\n0\n4\n"