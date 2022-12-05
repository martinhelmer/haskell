{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day18 where

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
   putStr " Part1: "
   readInp "input18.txt" >>= part1 >>= assertInt 4830
   --readInp "input18.txt" >>= part2 >>= assertInt 7621042



part1 ::String  -> IO Int
part1 s = do
    let a' = parseIntoArray s
        a = fillinDeadEnds a'
        char2Pos =  M.fromList $ map  (\(p,c) -> (c,p)) $ A.assocs a
        startPos = fst . head. filter (\(_,e) -> e=='@') $ A.assocs a
        missingKeys = filter isLower $ A.elems a
        ba =  reduceBigArray $ bigArray a
        -- ba =  bigArray a
        -- putStrLn $ draw2dchararr a
        -- putStrLn $ drawBigArray ba
        -- print startPos
        -- print missingKeys
        -- print $ findKeyDists ba missingKeys startPos
        -- let op =  findOnePath ba missingKeys startPos
        -- print op

    let (a,b,c) =  head $ dropWhile (\(_,_,s) -> "" /= missingkeys (PSQ.key $ fromJust (PSQ.findMin s))) . iterate (djikstra char2Pos ba) $ (M.empty, M.empty, PSQ.singleton (Node '@' missingKeys) 0)
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

findOnePath :: BigArr -> KeyList -> Pos -> [((Char, Pos), Int)]
findOnePath ba [] pos = []
findOnePath ba missingKeys pos = getNext :  findOnePath ba (delete keyFound missingKeys) keyPos
    where getNext = head $ findKeyDists ba missingKeys pos
          ((keyFound, keyPos), _) = getNext


type Distance = Int
type Key = Char
data Node = Node {key:: Key, missingkeys:: KeyList} deriving (Eq, Ord, Show)
type Visited = [Node]
type Pri = PSQ.PSQ Node Int
type NodeMap = M.Map Char Pos


neighbors :: M.Map Key Pos -> BigArr -> Node -> Int -> [(Node, Int)]
neighbors nm ba n nd= map (\((c,p),d) -> (Node c (delete c mising), d+nd) ) . findKeyDists ba mising $ pos
        where pos = nm M.! key n
              mising = missingkeys n


djikstra nodemap ba (dist, prev, psq) = (M.insert node nodeDist dist, updPrev, updPsq)
    where
        (binding, popped_psq) = fromJust $ PSQ.minView psq
        node = PSQ.key binding
        nodeDist = PSQ.prio binding
        nextOnes = neighbors nodemap ba node nodeDist
        needsUpdate = filter (\(n,d) -> fromMaybe (10^10) (PSQ.lookup n popped_psq) > d) nextOnes
        updPrev = mapInsert prev . map (\(n,_)->(n,node)) $ needsUpdate
        updPsq = foldl (\r (a,b) -> PSQ.insert a b r) popped_psq needsUpdate

