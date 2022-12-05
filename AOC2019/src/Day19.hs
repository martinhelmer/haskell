module Day19 where

import AOCHelper
import IntCode
import qualified Data.Map as M 
import qualified Data.Foldable as M
run :: IO ()
run = do
   putStrLn "--- Day 19: Tractor Beam ---"
   putStr " Part1: "
   readInp "input19.txt" >>= part1 >>= assertInt 189
   putStr " Part2: "
   readInp "input19.txt" >>= part2 >>= assertInt 7621042

part1 :: String -> IO Int 
part1 s = do -- not 2311 
     let comp = parseString s ::PComputer 
    --      tractorMap = M.fromList [((x,y),ask comp (x,y)) | x <- [0..49], y <- [0..49]]
    --  putStrLn ""
    --  putStrLn $ draw2dmap tractorMap 
    --  return $ M.length $  M.filter (==1) tractorMap
     return $ length . filter (==1) $ [ask comp (x,y) | x <- [0..49], y <- [0..49]]


ask :: PComputer -> (Int,Int) -> Int
ask c (x,y) = head $ output $ runComp $ addInp (addInp c x) y 


part2 :: String -> IO Int
part2 s = do 
    let c = parseString s ::PComputer 
        squareSize = 100
        ll = head $ filter (square c squareSize) (edges c)
        tlx = fst ll 
        tly = snd ll - (squareSize -1) 
    return $ 10000 * tlx + tly 

findLeftEdge c y x=  if ask c (x,y) == 1 then (x,y) else findLeftEdge c y (x+1)

findNextEdge c (x,y) = if ask c (x,y+1) == 1 then (x,y+1) else (x+1, y+1)

edges c = iterate (findNextEdge c) $ findLeftEdge c 50 25

square c n (x,y) = ask c (x+n-1, y-(n-1)) == 1