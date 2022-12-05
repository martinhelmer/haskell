{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day20 where
import AOCHelper
import qualified Data.Array as A
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import System.TimeIt

type Pos = (Int,Int)
type Layout =  A.Array (Int, Int) Char

cardinalDirs = [(-1,0),(1,0),(0,-1),(0,1)]

t01  = unlines ["         A           ",
                "         A           ",
                "  #######.#########  ",
                "  #######.........#  ",
                "  #######.#######.#  ",
                "  #######.#######.#  ",
                "  #######.#######.#  ",
                "  #####  B    ###.#  ",
                "BC...##  C    ###.#  ",
                "  ##.##       ###.#  ",
                "  ##...DE  F  ###.#  ",
                "  #####    G  ###.#  ",
                "  #########.#####.#  ",
                "DE..#######...###.#  ",
                "  #.#########.###.#  ",
                "FG..#########.....#  ",
                "  ###########.#####  ",
                "             Z       ",
                "             Z       "]

fillinDeadEnds :: (A.Ix a, A.Ix b, Num a, Num b) =>A.Array (a, b) Char -> A.Array (a, b) Char
fillinDeadEnds a = if null assocs then a else fillinDeadEnds (a A.// assocs)
   where assocs = map (\p -> (p,'#')) $ filter (\p -> a A.! p == '.' && length (dirsFromHere a p)==1) $ A.indices a


dirsFromHere a (x,y) | a A.! (x,y) == '#' = []
                     | otherwise = filter (\pp -> (a A.! pp) /='#') . map  (\(a,b) -> (a+x,b+y)) $ [(-1,0),(1,0),(0,1),(0,-1)]



portalExit :: Layout -> M.Map String [(Int, Int)] -> (Int, Int) -> (Int, Int)
portalExit a pm pos = head $ filter (/=pos) $  pm M.! fromJust (portal a pos)

tAdd (a,b) (c,d) = (a+c, b+d)

isPortal a pos =  isJust p && p `notElem` [Just "AA",Just "ZZ"]
    where p = portal a pos

portal :: Layout -> Pos -> Maybe String
portal a p@(y,x)
    | a A.! p /= '.' || y < 2 || x < 2 || y > (my-2) || x > (mx-2) = Nothing
    | isAlpha $ ixl (-1,0) = Just [ixl (-2,0), ixl (-1,0)]
    | isAlpha $ ixl  (1,0) = Just [ixl (1,0), ixl (2,0)]
    | isAlpha $ ixl (0,-1) = Just [ixl (0,-2), ixl (0,-1)]
    | isAlpha $ ixl (0,1) = Just [ixl (0,1), ixl (0,2)]
    | otherwise = Nothing

    where ixl d = a A.! tAdd p d
          (_,(my,mx) )= A.bounds a


portalMap :: Layout -> M.Map String [(Int, Int)]
portalMap a =  M.fromListWith (++)  $ mapMaybe (\p -> (, [p]) <$> portal a p) (A.indices a)

search :: Layout ->  M.Map String [(Int, Int)] -> Pos -> [Pos] -> Int -> Maybe Int
search a pm pos prev n
    | portal a pos == Just "ZZ" = Just n
    | otherwise = mymin $ mapMaybe (\p -> if p  `elem` prev then Nothing else search a pm p (pos:prev) (n+1)) branches
    where
        branches = if isPortal a pos then portalExit a pm pos : lookaround else lookaround
        lookaround = filter (\p -> (a A.! p) == '.' ) $ map (tAdd pos) cardinalDirs
        mymin [] = Nothing
        mymin l = Just $ minimum l


run :: IO ()
run = do
   putStrLn "--- Day 20: Donut Maze ---"
   putStr " Part1: "
   readInp "input20.txt" >>= part1 >>= assertInt 636


part1 s = do
    let a = parseIntoArray s
        pm = portalMap a 
        startpos = head $  pm M.! "AA"
    
    putStrLn $ draw2dchararr $ fillinDeadEnds a 
    timeIt $ return $ fromJust $ search a pm startpos [] 0
    
    
