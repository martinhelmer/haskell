{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Day24 (run) where

import Text.RawString.QQ ( r )
import AOCHelper ( assertIt, readInp, draw2dset, parseInto2dMap, mapBounds, draw2dmap, draw2dcharmap )
import Data.Maybe (mapMaybe, fromMaybe, isNothing)
import qualified Data.Map as OM
import qualified Data.HashMap as M
import qualified Data.HashSet as S
import qualified Data.Set as SS
import Data.Hashable (Hashable, hash)
import Data.List (zip4)
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))
import Debug.Trace (trace)
import Data.Bifunctor

newtype Pos = Pos (Int,Int) deriving (Eq, Ord, Show)
newtype Bounds = Bounds {bb::(Int,Int)} deriving (Eq,Ord, Show)
bx = fst . bb 
by = snd . bb
newtype Dir = Dir (Int, Int) deriving (Eq, Ord, Show)
newtype Wind = Wind (Int,Int) deriving (Eq, Ord, Hashable, Show)
newtype Winds = Winds (S.Set Wind) deriving (Eq, Ord)

instance Show Winds where 
    show (Winds w) = draw2dset $ SS.fromList $  map (\(Wind (a,b)) -> (a,b)) $  S.toList w  
-------------------------- ^ > v < 
data Storm = Storm Bounds Int (Winds,Winds,Winds,Winds) deriving (Eq, Ord)

cycsize (Storm b _ _) = cycsize' b 
cycsize' (Bounds (xm,ym)) = xm*ym

instance Show Storm where 
    show (Storm _ _ (Winds nw,Winds ew,Winds sw,Winds ww)) = draw2dcharmap  bm
        where nm =  OM.fromList . map (\(Wind w) -> (w,'^')) $ S.toList nw
              em =  OM.fromList . map (\(Wind w) -> (w,'>')) $ S.toList ew
              sm =  OM.fromList . map (\(Wind w) -> (w,'v')) $ S.toList sw 
              wm =  OM.fromList . map (\(Wind w) -> (w,'<')) $ S.toList ww 
              bm =  OM.unionsWith (\_ _ -> '2') [nm, em, sm, wm] 

dirsNESW = [dirN, dirE, dirS, dirW]
dirN = Dir (0,-1)
dirE = Dir (1,0)
dirS = Dir (0,1)
dirW = Dir (-1,0)


t2l (a,b,c,d) = [a, b,c,d]
l2t [a,b,c,d] = (a,b,c,d)

moveWind :: Bounds -> Dir -> Wind -> Wind
moveWind b (Dir (dx, dy)) (Wind (wx,wy)) = 
        Wind ((wx+dx) `mod` bx b, (wy+dy) `mod` by b)

moveWinds :: Bounds -> Dir -> Winds -> Winds
moveWinds b d (Winds winds) = Winds $ S.map (moveWind b d) winds

repeatingWinds :: Bounds -> Dir -> Winds -> [Winds]
repeatingWinds b d w =  cycle $ take x $ iterate (moveWinds b d) w 
                        where x | d == dirW || d == dirE  = bx b
                                | otherwise = by b 

repeatingStorm' ::Storm -> [Storm]
repeatingStorm' (Storm b n (nw,ew,sw,ww)) = 
    zipWith (\n' w -> Storm b ((n+n')`mod` cycsize' b) w ) 
                        [0..]
                        $ zip4  (repeatingWinds b dirN nw)
                                (repeatingWinds b dirE ew)
                                (repeatingWinds b dirS sw)
                                (repeatingWinds b dirW ww) 

parseStorm :: String -> Storm
parseStorm s = Storm b 0 (filteron '^', filteron '>', filteron 'v', filteron '<')   
        where m = parseInto2dMap s
              filteron c = Winds . S.map (\(x,y) -> Wind (x-1,y-1)) . S.fromList . OM.keys . OM.filter (==c) $ m
              ((_,_),(mx,my)) = mapBounds (OM.keys m)
              b = Bounds (mx-1,my-1)


--- bfs starts here ... vvv
ep :: Bounds -> (Int, Int)
ep b = (bx b-1, by b)

isFree :: Storm -> Pos -> Bool 
isFree (Storm b@(Bounds (mx,my)) _ (Winds nw,Winds ew,Winds sw,Winds ww)) (Pos (x,y)) 
                | x == 0 && y == -1 = True           -- start
                | (x,y) == ep b  = True              -- end 
                | x < 0 || y < 0 = False             -- out of bounds
                | x >= mx || y >= my = False         -- out of bounds
                | otherwise =  not . any ( S.member (Wind (x,y))) $ [nw, ew, sw, ww]

availablePositons :: Storm -> Pos -> [Pos]
availablePositons s (Pos (x,y)) = 
    filter (isFree s) $ map (\(Dir (dx, dy)) -> Pos (x+dx,y+dy) ) [dirE, dirS, dirN, dirW, Dir (0,0)]
  
neighbors :: Node -> [Node]
neighbors  (Node (pos,_:s2:xs)) = map (\p -> Node (p,s2:xs)) . availablePositons s2 $ pos  

newtype Visited = Visited (Pos,Int) deriving (Eq, Ord, Show)
newtype Node = Node (Pos, [Storm]) deriving (Eq, Ord)

instance Show Node where 
    show (Node (p , x:_)) = show p ++ "\n" ++ show x 

n2v :: Node -> Visited
n2v  (Node (pos, (Storm _ i _):_)) = Visited (pos,i)


bfsstart' :: ( Node -> [Node]) -> (Node->Bool) -> Node -> (Maybe Node, OM.Map Visited Visited)
bfsstart' nf df node = bfs' nf df (OM.singleton (n2v node) (n2v node)) (Seq.singleton node)

bfs' :: ( Node -> [Node]) -> (Node -> Bool) ->  OM.Map Visited Visited -> Seq Node -> (Maybe Node, OM.Map Visited Visited)
bfs' nf df visited queue | Seq.empty == queue = (Nothing, visited)
                         | df node = (Just node, visited) 
                         | otherwise = bfs' 
                                        nf 
                                        df 
                                        (OM.union 
                                            visited 
                                            (OM.fromList (map (\n -> (n2v n,n2v node)) neighbors')
                                            )) 
                                        ((><) (Seq.fromList neighbors') popped)
                       where (popped Seq.:|> node) = queue 
                             neighbors' = filter (\a -> not $ OM.member (n2v a) visited) (nf node) 

bfslen :: Num a => (Maybe Visited, OM.Map Visited Visited) -> a
bfslen (Just node, visited) | parent == Just node = 0
                       | otherwise = 1 + bfslen (parent, visited)
                       where parent = OM.lookup node visited
                  
startNode :: String -> Node
startNode s =  Node (Pos (0,-1) ,  repeatingStorm' .  parseStorm $ s)

nodeIsAtEnd :: Node -> Bool
nodeIsAtEnd (Node (pos,(Storm b _ _ ):_)) = pos == Pos (ep b)

nodeIsAtStart :: Node -> Bool
nodeIsAtStart (Node (pos,_)) = pos == Pos (0,-1)


part1 :: String -> IO Int
part1 s = do 
    let l = bfsstart' neighbors nodeIsAtEnd (startNode s)
    -- print $ OM.size (snd l)
    return $ bfslen $ first (n2v <$>) $  l

part2 :: String -> IO Int
part2 s = do
    return $ fst 
             . foldl (\(l,n) df -> 
                        let (Just n1,v1) = bfsstart' neighbors df n 
                        in (l+ bfslen (Just (n2v n1),v1), n1)) 
              (0,startNode s) 
              $ [nodeIsAtEnd, nodeIsAtStart, nodeIsAtEnd]


run :: IO ()
run = do
  putStrLn "--- Day24 Winds ---"
  putStr " Part1: "
  readInp  "input24.txt" >>= part1 >>= assertIt 301 
  putStr " Part2: "
  readInp "input24.txt" >>= part2 >>= assertIt 859 

runtest :: IO ()
runtest = do 
     part1 test1 >>= assertIt 18 

test1 :: String
test1 = [r|#E######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#|]

test2 = [r|#E##
#..#
##.#|]


tstorms = repeatingStorm' $ parseStorm test1 


