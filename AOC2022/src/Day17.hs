{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day17 (run) where

import AOCHelper ( assertIt, readInp)
import qualified Data.Set as S
import Data.Maybe ( fromJust )




rockH = S.fromList [(0,0),(0,1),(0,2),(0,3)]
rockPlus = S.fromList [(-2,1),(-1,0),(-1,1),(-1,2),(0,1)]
rockL = S.fromList [(-2,2),(-1,2),(0,0),(0,1),(0,2)]
rockV = S.fromList [(-3,0),(-2,0),(-1,0),(0,0)]
rockSQ = S.fromList [(-1,0),(-1,1),(0,0),(0,1)]

startblocks = cycle [rockH, rockPlus, rockL, rockV, rockSQ]

type Blocks = [S.Set (Int, Int)]
type Wind = [(Int,Char)] 
type Rock = S.Set (Int,Int)
type Cave = S.Set (Int,Int)


initCave = S.fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6)]

--  

tplus (x1,y1) (x2,y2) = (x1+x2,y1+y2)

move :: Cave -> Rock -> (Int,Int) -> Maybe Rock
move cave rock d = case d of 
    (1,0) -> if  no_overlaps then Just newrock else Nothing
    (0,-1) -> if (minimum . map snd $ rockelems) > 0 && no_overlaps then Just newrock else Just rock
    (0,1) -> if (maximum  . map snd $ rockelems) < 6 && no_overlaps then Just newrock else Just rock  
    _ -> error "This should not happen"
   where rockelems = S.elems rock 
         newrock  = S.map (tplus d) rock
         no_overlaps = S.intersection cave newrock == S.empty

droprock :: (Int, Cave, Wind, Blocks) -> (Int, Cave, Wind, Blocks)
droprock (count, cave, wind , (rock:blocks)) = (count+1, S.union cave finalrock, finalwind ,blocks)
    where initpos = S.map (tplus ((fst . S.findMin $ cave) -4,2)) rock 
          (finalwind, finalrock) = dr wind initpos
          dr ((_,x):xs) rp = if done then (xs, r) else dr xs r 
                            where (done, r) = blowndrop cave rp x

blowndrop :: Cave -> Rock -> Char -> (Bool, Rock)
blowndrop cave rock winddir =  maybe (True,dowind) (False,) (move cave dowind (1,0))
          where dowind = fromJust (move cave rock (d winddir)) 
                d '<' = (0,-1)
                d '>' = (0,1)
                d _ = undefined

height c =  - (fst $ S.findMin c)

part1 :: String  -> IO Int
part1 s  = do
  let wind = cycle $ zip [0..] (init s) 
      (_, c, _,_) = iterate droprock (0, initCave, wind, startblocks)  !! 2022
  -- putStrLn $ draw2dset (S.map (\(a,b) -> (b,a)) c) 
  return $ height c


findblock :: Wind -> (Int, Int) 
findblock wind = (fst (blocks !! 1)  - fst (blocks !! 0), snd (blocks !! 1)  - snd (blocks !! 0) ) 
    where (_, _, mw:_ ,r:_) = iterate droprock (0, initCave, wind, startblocks)  !! 5555
          ff (count, cave, (w:_), (b:_)) = w == mw && b == r 
          blocks = map (\(count, cave, w:_, b:_) -> (height cave,count ))  (filter ff (iterate droprock (0, initCave, wind, startblocks)))
 
part2 ::  String -> IO Int
part2 s = do 
    let wind = cycle $ zip [0..] (init s) 
    
        totalrocks = 1000000000000
        (blockheight, blockrocks) = findblock wind 
        remrocks = totalrocks `rem` blockrocks 
        blockrounds = totalrocks `div` blockrocks 
        roundheighs = blockheight * (blockrounds -1) 
        remheight = (\(_, c, _,_) -> height c) $ iterate droprock (0, initCave, wind, startblocks)  !! (remrocks + blockrocks)

    return $ roundheighs + remheight

run :: IO ()
run = do
   putStrLn "--- Day17 Tetris ---"
   putStr " Part1: "
   readInp  "input17.txt" >>= part1 >>= assertIt 3059
   putStr " Part2: "
   readInp  "input17.txt" >>= part2 >>= assertIt 1500874635587


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 3068
     part2 test >>= assertIt 1514285714288

test :: String
test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"