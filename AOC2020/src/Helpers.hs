module Helpers where

import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Data.Bifunctor

type Array2D =  Array.Array (Int,Int)

ints = map (\x -> read x ::Int ) . lines


splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                               | otherwise = (c:x):xs

-- [[.,.,.],
--  [.,.,.]] ->    (2x3) map     
listList2map ::  (a->b) -> [[a]] -> Map.Map (Int,Int) b
listList2map f l = Map.fromList $ zip (ixs . listListDim $ l)  (map f . concat $ l)

listList2array ::  (a->b) -> [[a]] -> Array2D b
listList2array f l = Array.listArray ((1,1), listListDim l) (map f . concat $ l)


listListDim :: [[a]] -> (Int,Int)
listListDim  l = (length l, length $ head l)

ixs (r,c) = [(a,b) | a <- [1..r], b <- [1..c]]

-- space is (1,1),(rows,cols)
rLeftIx :: (Int,Int) -> (Int, Int) -> (Int,Int)
rLeftIx (rows, cols) (row,col) = (cols - col + 1 , row)

-- space is (1,1),(rows,cols)
flipIx :: (Int,Int) -> (Int, Int) -> (Int,Int)
flipIx (rows, _) (row,col) = (rows - row + 1 , col)


-- assuming bounds = (1,1),(row,cols)
rLeftArray ::Array2D a -> Array2D a
rLeftArray  arr = Array.array ((1,1),(cols,rows)) $ map (Data.Bifunctor.first (rLeftIx (rows, cols))) $ Array.assocs arr
     where (rows,cols) = snd $ Array.bounds arr

flipArray  ::Array2D a -> Array2D a
flipArray  arr = Array.array ((1,1),(rows,cols)) $ map (Data.Bifunctor.first (flipIx (rows, cols))) $ Array.assocs arr
     where (rows,cols) = snd $ Array.bounds arr