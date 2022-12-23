{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
module MyBfs (bfsstart, bfslen) where 
    
import qualified Data.Map as M
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))



bfsstart :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> (Maybe a, M.Map a a)
bfsstart nf df node = bfs nf df (M.singleton node node) (Seq.singleton node)

bfs :: (Ord a) => (a -> [a]) -> (a -> Bool) -> M.Map a a -> Seq a -> (Maybe a,  M.Map a a) 
bfs nf df visited queue | Seq.empty == queue = (Nothing, visited)
                        | df node = (Just node, visited) 
                        | otherwise = bfs nf df (M.union visited (M.fromList (map (,node) neighbors))) ((><) (Seq.fromList neighbors) popped)
                       where (popped Seq.:|> node) = queue 
                             neighbors = filter (\a -> not $ M.member a visited) (nf node) 

bfslen :: (Num a2, Ord a1) => (Maybe a1, M.Map a1 a1) -> a2
bfslen (Just node, visited) | parent == Just node = 0
                       | otherwise = 1 + bfslen (parent, visited)
                       where parent =  M.lookup node visited

