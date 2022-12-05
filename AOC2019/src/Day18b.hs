{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day18b where

import AOCHelper
import qualified Data.Array as A
import qualified Data.Map as M

import Data.Char
import System.TimeIt
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Control.Applicative as Set
import qualified  Data.PSQueue as PSQ

type MyArr =  A.Array (Int, Int) Char
type BigArr = A.Array (Int,Int) (Char,[(Int,Pos)])
type Pos = (Int,Int)
type KeyList = [Char]

t2 = unlines ["########################",
            "#...............b.C.D.f#",
            "#.######################",
            "#.....@.a.B.c.d.A.e.F.g#",
            "########################"]
t3 = unlines ["#################",
            "#i.G..c...e..H.p#",
            "########.########",
            "#j.A..b...f..D.o#",
            "########@########",
            "#k.E..a...g..B.n#",
            "########.########",
            "#l.F..d...h..C.m#",
            "#################"]

t3b = unlines ["#################",
              "#i.G..c...e..H..#",
              "########.########",
              "#j....b...f..D..#",
              "########@########",
              "#k.E......g..B..#",
              "########.########",
              "#l.F..d...h..C..#",
              "#################"]

t4 = unlines ["########################",
            "#@..............ac.GI.b#",
            "###d#e#f################",
            "###A#B#C################",
            "###g#h#i################",
            "########################"]

fillinDeadEnds a = if null assocs then a else fillinDeadEnds (a A.// assocs)
   where assocs = map (\p -> (p,'#')) $ filter (\p -> a A.! p == '.' && length (dirsFromHere a p)==1) $ A.indices a

bigArray :: MyArr -> BigArr
bigArray a = A.listArray (A.bounds a) $ map ml $ A.indices a
        where ml p = (a A.! p, map (\p -> (1,p)) $ dirsFromHere a p)

reduceBigArray :: BigArr -> BigArr
reduceBigArray ba | null fassocs = ba
                  | otherwise = reduceBigArray $ ba A.// [(pos,(' ',[])),(pos2,updval pos2 pos1 (l2pos1 + l2pos2)), (pos1,updval pos1 pos2 (l2pos1 + l2pos2))]
    where fassocs = filter (\(_,(symb, l)) -> symb == '.' && (length l == 2) ) $ A.assocs ba
          (pos, (_,[x, y])) = head fassocs
          (l2pos1, pos1) =x
          (l2pos2, pos2) =y
          updval p newp d = (s', map (\(d',pn) -> if pn /= pos then (d',pn) else (d,newp)) l' )
              where (s',l') = ba A.! p

drawBigArray ba = draw2dcharmap $ M.fromList $ map (\(ix,(c,_)) -> (ix,c)) $ A.assocs ba


dirsFromHere a (x,y) | a A.! (x,y) == '#' = []
                     | otherwise = filter (\pp -> (a A.! pp) /='#') . map  (\(a,b) -> (a+x,b+y)) $ [(-1,0),(1,0),(0,1),(0,-1)]

moves :: BigArr -> [Pos] -> [Char] -> Pos -> [(Int,Pos)]
moves a visited missingKeys p =  filter isok $ snd (a A.! p)
            where isok (_,p) | p `elem` visited = False
                             | isUpper v && ( toLower v `elem` missingKeys) = False
                             | otherwise = True
                    where v = fst $ a A.! p
a = do
    s <- readInp "input18.txt"
    return $ parseIntoArray s


run :: IO ()
run = do
   putStrLn "--- Day 18: Many-Worlds Interpretation ---"
   putStr " Part2: "
   readInp "input18.txt" >>= part1 >>= assertInt 1946


part1 ::String  -> IO Int
part1 s = do
    let a' = parseIntoArray s
        a = fillinDeadEnds a'
        char2Pos' =  M.fromList $ map  (\(p,c) -> (c,p)) $ A.assocs a
        midpos = char2Pos' M.! '@'
        a'' = a A.// (map (\((a,b),c) -> ((a+fst midpos, b+snd midpos),c)) $
             [((-1,-1),'1'), ((0,-1),'#'),((1,-1),'2'),
              ((-1,0),'#'), ((0,0),'#'),((1,0),'#'),
              ((-1,1),'3'), ((0,1),'#'),((1,1),'4')])
        char2Pos =  M.fromList $ map  (\(p,c) -> (c,p)) $ A.assocs a''

        missingKeys = filter isLower $ A.elems a''
        ba =  reduceBigArray $ bigArray a''
    putStrLn $ drawBigArray ba
    let fn = neighbors2 char2Pos ba 
        (a,b,c) =  head $ dropWhile (\(_,_,s) -> "" /= missingkeys (PSQ.key $ fromJust (PSQ.findMin s))) . 
            iterate (djikstra fn) $ (M.empty, M.empty, PSQ.singleton (Node2 '1' '2' '3' '4' missingKeys) 0)
        endnode = fromJust $ PSQ.findMin c
    return $ PSQ.prio endnode


findKeyDists :: BigArr -> KeyList -> Pos -> [((Char, Pos), Int)]
findKeyDists ba missingKeys p  =  M.toList $ M.fromListWith min $ map dist $ findKeys ba [] missingKeys (0,p)

findKeys :: BigArr -> [Pos] -> KeyList -> (Int,Pos) -> [[(Char,Int,Pos)]]
findKeys ba visited missingKeys p | isLower symb && symb `elem` missingKeys= [[(symb,fst p, snd p)]]
                                  | otherwise = concatMap q (moves ba visited missingKeys (snd p))
            where symb = fst $  ba A.! snd p
                  q =  map ((symb,fst p, snd p):) . findKeys ba (snd p:visited) missingKeys

dist :: [(Char,Int,Pos)] -> ((Char,Pos), Int)
dist l = (cp $ last l , sum (map (\(_,i,_) -> i) l))
    where cp (c,i,p) = (c,p)


type Distance = Int
type Key = Char
data Node = Node2 {key1:: Key, 
                  key2:: Key,
                  key3:: Key,
                  key4:: Key,
                  missingkeys:: KeyList} deriving (Eq, Ord, Show)
type Visited = [Node]
type Pri = PSQ.PSQ Node Int
type NodeMap = M.Map Char Pos


neighbors2 :: M.Map Key Pos -> BigArr -> Node -> Int -> [(Node, Int)]
neighbors2 nm ba (Node2 k1 k2 k3 k4 mk) nd= 
        (map (\((c,p),d) -> (Node2 c k2 k3 k4 (delete c mk), d+nd) ) . findKeyDists ba mk $ (nm M.! k1)) ++ 
        (map (\((c,p),d) -> (Node2 k1 c k3 k4 (delete c mk), d+nd) ) . findKeyDists ba mk $ (nm M.! k2)) ++
        (map (\((c,p),d) -> (Node2 k1 k2 c k4 (delete c mk), d+nd) ) . findKeyDists ba mk $ (nm M.! k3)) ++
        (map (\((c,p),d) -> (Node2 k1 k2 k3 c (delete c mk), d+nd) ) . findKeyDists ba mk $ (nm M.! k4)) 

djikstra fn (dist, prev, psq) = (M.insert node nodeDist dist, updPrev, updPsq)
    where
        (binding, popped_psq) = fromJust $ PSQ.minView psq
        node = PSQ.key binding
        nodeDist = PSQ.prio binding
        nextOnes = fn node nodeDist
        needsUpdate = filter (\(n,d) -> fromMaybe (10^10) (PSQ.lookup n popped_psq) > d) nextOnes
        updPrev = mapInsert prev . map (\(n,_)->(n,node)) $ needsUpdate
        updPsq = foldl (\r (a,b) -> PSQ.insert a b r) popped_psq needsUpdate

