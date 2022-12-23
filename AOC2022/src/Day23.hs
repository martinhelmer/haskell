{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day23 (run) where

  import Text.RawString.QQ ( r )
  import AOCHelper ( assertIt, readInp, draw2dset )
  import Data.Maybe (mapMaybe, fromMaybe, isNothing)
  import qualified Data.HashMap as M
  import qualified Data.HashSet as S
  import qualified Data.Set as SS 
  type Dir = (Int,Int)
  type Elf = (Int,Int)
  type Elves = S.Set Elf
  type Order = [Dir]
  type Propositions = M.Map Elf Elf

  orderlookup :: Dir -> [Dir]
  orderlookup d = case d of 
              (0,-1) -> [(-1,-1), ( 0,-1), ( 1,-1)] --- north
              (-1,0) -> [(-1,-1), (-1, 0), (-1, 1)] -- west
              (0, 1) -> [(-1, 1), ( 0, 1), ( 1, 1)] --- south
              ( 1,0) -> [( 1,-1), ( 1, 0), ( 1, 1)] -- east
              d' -> error ("undefined dir in order lookup" ++ show d')

  initorder :: [Dir]
  initorder = [(0,-1), (0,1),(-1,0), (1,0)]

  tp (a,b) (c,d) = (a+c,b+d)

  elfMove :: Propositions -> Elf -> Elf
  elfMove props elf = fromMaybe elf (moveifsafe elfprop) 
          where elfprop = M.lookup elf props
                moveifsafe Nothing = Nothing 
                moveifsafe (Just (dx,dy)) | isNothing nl = Just $ tp elf (dx,dy)
                                          | nl /= Just (-dx,-dy) = Just $ tp elf (dx,dy)
                                          | otherwise=  Nothing 
                              where nl = M.lookup (tp elf (2*dx, 2*dy)) props
  
  elfProposition :: Elves -> Order -> Elf -> Maybe Elf 
  elfProposition elves order elf | null neighbors = Nothing 
                                | otherwise = proposition order
        where neighbors =  mapMaybe 
                            (\e -> if S.member (tp elf e) elves then Just e else Nothing) 
                            [(x,y) | x<-[-1..1], y<-[-1..1] , x /=0 || y /=0]
              proposition [] = Nothing 
              proposition (x:xs) =  if any (`elem` neighbors) (orderlookup x) 
                                        then proposition xs 
                                        else Just x 

  elfPropositions :: Elves -> Order ->Propositions
  elfPropositions elves order = M.fromList 
                              $  mapMaybe (\e -> (e,) <$> elfProposition elves order e)  
                                  (S.elems elves)

  parseElves :: String -> Elves
  parseElves s  = S.fromList 
                . mapMaybe (\(p,v) -> if v == '#' then Just p else Nothing) 
                . zip ([(x,y) | y <- [0..(rows-1)] , x <-[0..(cols-1)]]) $ concat lns
                where lns  = lines s
                      rows = length lns
                      cols = length . head $ lns

  moveelves n elves order 
                      | n == 0 || M.size props == 0 = (n,elves) 
                      | otherwise = moveelves (n-1) (S.map (elfMove props) elves) (rotate order)
                      where rotate (x:xs) = xs ++[x]
                            props = elfPropositions elves order

  part1 :: String -> IO Int
  part1 s = do
    let elves = parseElves s  
    -- putStrLn "==========================="
    -- putStrLn $ draw2dset elves 
    -- putStrLn ""
    let (n,sol) = (moveelves 10 elves initorder)
    let s =  draw2dset  $ SS.fromList $ S.toList sol 
    return $ length . filter (=='.') $ s 
    
  part2 :: String -> IO Int
  part2 s = do
    let elves = parseElves s  
    -- putStrLn "==========================="
    -- putStrLn $ draw2dset elves 
    -- putStrLn ""
    let (n,sol) = (moveelves 1000 elves initorder)
    return $ 1 + 1000 - n 



  run :: IO ()
  run = do
    putStrLn "--- Day23 ---"
    putStr " Part1: "
    readInp  "input23.txt" >>= part1 >>= assertIt 4181 
    putStr " Part2: "
    readInp "input23.txt" >>= part2 >>= assertIt 973 

runtest :: IO ()
runtest = do 
     part1 test1 >>= assertIt 3
     part1 test2 >>= assertIt 110
     part2 test1 >>= assertIt 4
     part2 test2 >>= assertIt 20

test1 = [r|.....
..##.
..#..
.....
..##.
.....|]

test2 :: String 
test2 = [r|....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..|]





