{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TupleSections #-}

module Day12 (run) where

import Data.Sequence (Seq, ViewR(..), ViewL(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as S 
import AOCHelper ( assertIt, readInp, parseIntoArray)
import qualified Data.Array as A
import Data.Array (Array, (!))
import qualified Data.Map as M
import Data.Char (ord, chr)
import Data.Maybe ( fromJust ) 
import Data.Containers.ListUtils ( nubOrd )
import GHC.IO (noDuplicate)
-- import Debug.Trace

-- visitableFrom :: Heights -> Visited -> (Int, Int) ->Visitable
-- visitableFrom a visited ix = map (,h) $ filter go $ cardinals (A.bounds a) ix 
--     where go ix2 = (not . M.member ix2 $ visited ) && (height (a ! ix2) - height (a ! ix)) <= 1 
--           h = 1 + fromJust ( M.lookup ix visited)

-- doVisits :: Heights -> Visited -> Visitable-> (Visited, Visitable)
-- doVisits a visited  toVisit = (newVisited, newTovisit)
--   where newVisited = M.union visited (M.fromList toVisit)
--         newTovisit = nubOrd . concatMap (visitableFrom a newVisited . fst) $ toVisit

-- go :: Heights ->(Visited, Visitable) -> Int 
-- go a (visited, tovisit) | not (null has_e) =  snd (head has_e)
--                         | otherwise = go a (doVisits a visited tovisit)
--       where has_e = filter (\(k,_) ->(a ! k) == 'E') tovisit

-- shortest :: Heights ->  Int
-- shortest a =  go a (visited, toVisit)
--                   where start = fst . head . filter (\x -> snd x == 'S') $ A.assocs a
--                         visited = M.singleton start 0 
--                         toVisit = visitableFrom a visited start 

-- type Heights = Array (Int, Int) Char
-- type Visited =  M.Map (Int,Int) Int
-- type Visitable = [((Int, Int),Int)]

height :: Char -> Int 
height 'S' = height 'a'
height 'E' = height 'z'
height c = ord c - ord 'a'

inBound :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> (a1, a2) -> Bool
inBound  ((x0,y0),(xm,ym)) (x,y) = x >= x0 && x <= xm && y >= y0 && y <= ym

cardinals :: (Ord a1, Ord a2, Num a1, Num a2) => ((a1, a2), (a1, a2)) -> (a1, a2) -> [(a1, a2)]
cardinals a (x,y) = filter (inBound a) [ (x+1,y), (x-1,y) , (x,y+1), (x,y-1)]

flipE :: Char -> Char
flipE 'E' = 'S'
flipE 'S' = 'E'
flipE 'a' = 'E'
flipE c = chr $ (ord 'z' - ord c) + ord 'a'

flippedArray :: A.Ix i => Array i Char -> Array i Char
flippedArray a = A.listArray (A.bounds a) (map flipE $ A.elems a)


nf a node = filter (\ix2 -> (height (a ! ix2) - height (a ! node)) <= 1 ) (cardinals (A.bounds a) node)

dobfs a = bfslen $ bfsstart (nf a) ((\a k -> (a ! k) == 'E') a)  start
    where start = fst . head . filter (\x -> snd x == 'S') $ A.assocs a

part1 :: String -> IO Int
part1 s = do
  return $ dobfs .  parseIntoArray $ s
  -- return $ shortest . parseIntoArray $ s

part2 :: String -> IO Int
part2 s = do
  return $  dobfs . flippedArray . parseIntoArray $ s
  -- return $ shortest . flippedArray . parseIntoArray $ s


run :: IO ()
run = do
   putStrLn "--- Day12 Mountain Strolling ---"
   putStr " Part1: "
   readInp  "input12.txt" >>= part1 >>= assertIt 350
   putStr " Part2: "
   readInp "input12.txt" >>= part2 >>= assertIt 349

test :: String
test = unlines ["Sabqponm",
                "abcryxxl",
                "accszExk",
                "acctuvwj",
                "abdefghi"]

bfsstart :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> (Maybe a, M.Map a a)
bfsstart nf df node = bfs nf df (M.singleton node node) (Seq.singleton node)

bfs :: (Ord a) => (a -> [a]) -> (a -> Bool) -> M.Map a a -> Seq a -> (Maybe a,  M.Map a a) 
bfs nf df visited queue | Seq.empty == queue = (Nothing, visited)
                        | df node = (Just node, visited) 
                        | otherwise = bfs nf df (M.union visited (M.fromList (map (,node) neighbors))) ((><) (Seq.fromList neighbors) popped)
                       where (popped Seq.:|> node) = queue 
                             neighbors = filter (\a -> not $ M.member a visited) (nf node) 

bfslen :: (Num a2, Ord a1) => (Maybe a1, M.Map a1 a1) -> a2
bfslen (Just node, visited) | parent == node = 0
                       | otherwise = 1 + bfslen (Just parent, visited)
                       where parent =fromJust $ M.lookup node visited