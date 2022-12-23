{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day14 (run) where

import AOCHelper ( assertIt, readInp)
import  Data.List.Split (splitOn)
import Data.Containers.ListUtils ( nubOrd )
import qualified Data.Map as M 
import Data.Maybe ( isNothing, isJust, fromJust, mapMaybe ) 
import MyBfs ( bfsstart ) 

s2t s = (read a, read b )
    where [a,b] = splitOn "," s 

parseRow s = nubOrd .
             concatMap (\((x1,y1),(x2,y2)) -> [(i,j) | i <- [min x1 x2 .. max x1 x2 ],  j <- [min y1 y2 .. max y1 y2]] ) $ zip l (tail l )
                where  l =  map s2t $ splitOn " -> " s

doGrain :: Int -> ((Int, Int) -> Maybe (Int, Int)) ->  Maybe (M.Map (Int, Int) Int) -> Maybe  (M.Map (Int, Int) Int) 
doGrain floor' floorfunc m' | m == M.empty = Nothing
                          | isJust$ look (500,0) = Nothing
                          | otherwise = (\k -> M.insert k 2 m) <$> getstop (500,0)
                            where m = fromJust m' 
                                  look k = M.lookup k m
                                  getstop (x,y)  | y > floor' = floorfunc (x,y) 
                                                 | isNothing (look (x,y+1)) = getstop (x, y+1)
                                                 | isNothing (look (x-1,y+1)) = getstop (x-1, y+1)
                                                 | isNothing (look (x+1,y+1)) = getstop (x+1, y+1)
                                                 | otherwise = Just (x,y)


parseLines :: String -> [(Int, Int)]
parseLines s = nubOrd . concat . map parseRow $ lines s 


part1 :: String -> IO Int
part1 s = do  
  let m = M.fromList $ map (,1) $ parseLines s 
      mfloor = maximum  . map snd $ M.keys m

  -- putStrLn $ draw2dmap $ fromJust $ iterate (doGrain mfloor (const Nothing)) (Just m) !! 3
  return $ length (takeWhile isJust $ iterate (doGrain mfloor (const Nothing)) (Just m) ) -1 

part2 :: String -> IO Int
part2 s = do  
  let m = M.fromList $ map (,1) $ parseLines s 
      mfloor = maximum  . map snd $ M.keys m
  return $ length ( takeWhile isJust $ iterate (doGrain mfloor Just) (Just m) ) -1 


run :: IO ()
run = do
   putStrLn "--- Day14 Sand ---"
   putStr " Part1: "
   readInp  "input14.txt" >>= part1 >>= assertIt 1330
  --  putStr " Part2: "
  --  readInp "input14.txt" >>= part2 >>= assertIt 26139
   putStr " Part2: BFS: "
   readInp "input14.txt" >>= part2b >>= assertIt 26139

test = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 24
     part2 test >>= assertIt 93
     part2b test >>= assertIt 93 

part2b :: String -> IO Int
part2b s = do  
  let m = M.fromList $ map (,1::Int) $ parseLines s 
      mfloor = maximum  . map snd $ M.keys m
  let (_,visited) = bfsstart (neigh m mfloor) (const False) (500,0)
  return $ M.size visited 

neigh :: (Ord a1, Ord a2, Num a1, Num a2) => M.Map (a2, a1) a3 -> a1 -> (a2, a1) -> [(a2, a1)]
neigh m fl (x,y) = mapMaybe (\k@(_,y') -> if (y' < (fl + 2 )) && isNothing (M.lookup k m) then Just k else Nothing) 
                               [(x-1,y+1), (x,y+1), (x+1,y+1)]

