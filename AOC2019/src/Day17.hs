module Day17 where
import AOCHelper
import IntCode
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

run :: IO ()
run = do
   putStrLn "--- Day 17: Set and Forget ---"
--    putStr " Part1: "
--    readInp "input17.txt" >>= part1 >>= assertInt 5394
   putStr " Part2: "
   readInp "input17.txt" >>= part2 >>= assertInt 5394


part1 :: String -> IO Int
part1 s = do
    let c = parseString s :: PComputer
        grid = init $ map chr $ reverse $  output $ runComp c
    let m = parseInto2dMap grid
    putStrLn ""
    putStrLn $ draw2dcharmap m
    let intersections = filter ( isIntersection m) $ M.keys m
    print intersections
    print $ sum $ map (uncurry (*)) intersections
    return 0



isIntersection m (x,y) = all  (=='#')  q
    where q = map (\p -> fromMaybe ' ' (M.lookup  p m) ) ([(x+dx,y+dy) |(dx,dy) <- [(0,-1),(0,1),(1,0),(-1,0),(0,0)]])


-- A L,8,R,10,L 10 ,
-- B R,10,L,8,L,8,L,10,
-- A  L,8,R,10,L,10,
-- C L,4,L,6,L,8,L8,
-- B R,10,L,8,L8,L,10,
-- C L,4,L,6,L,8,L,8,
-- A L8 R,10,L,10,
-- C L,4,L,6,L,8,L8,
-- B R,10,L,8,L,8L,10,
-- C L,4,L,6,L,8,L8


sol = map ord . unlines $ ["A,B,A,C,B,C,A,C,B,C",
                         "L,8,R,10,L,10",
                         "R,10,L,8,L,8,L,10",
                         "L,4,L,6,L,8,L,8",
                         "n"]

part2 :: String -> IO Int
part2 s = do
    let c = runComp $ setInp (setInstr (parseString s :: PComputer) 0 2 ) sol
    putStr $ showOut c  
    print c 
    putStrLn ""
    return 0
    

lsss :: IO [Int]
lsss =  readLn