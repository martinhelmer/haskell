{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Day22 (run) where

import Text.RawString.QQ ( r )
import Data.List ( foldl', transpose ) 
import AOCHelper ( assertIt, readInp, parseIntoArray, draw2dchararr)
import Data.Maybe ( fromJust, mapMaybe, isJust )
import qualified Data.Map as M
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))
import Text.ParserCombinators.ReadP
import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( isDigit, isAlpha ) 
import Text.Read (readMaybe)
import  Data.List.Split (splitOn)
import qualified Data.Array as A 
import qualified Data.Vector as V 
import Debug.Trace (trace)

-- Parse 

spaces = munch (== ' ')

instr :: ReadP Instr
instr = (Turn <$> satisfy isAlpha) <|> (Move <$> number)


number :: ReadP Int
number = do
   c <-  satisfy isDigit
   b <-  munch (\c -> isDigit c)
   return $ read (c:b) 
   

parseRow :: String -> [Instr]
parseRow = fst . last . readP_to_S (many instr)

type P = (Int,Int)
type Bound = (Int, Int)

data Pstate = Pstate {pp::P,  pdir::P} deriving (Show)

data Instr = Turn Char | Move Int deriving (Show)

data Path = Path {patha::(A.Array P Char), pathrb::(V.Vector Bound),  pathcb::(V.Vector Bound) } deriving (Show)

turn :: P -> Char -> P
turn (dr,dc) c = case c of 
                  'R' -> (dc,-dr)
                  'L' -> (-dc,dr) 
                  _ -> undefined

-- right = (0,1)
-- left = (0,-1)
-- up = (-1,0)
-- down = (1,0)

tp :: P -> P -> P
tp (a,b) (c,d) = (a+c,b+d)

moveinstr :: Path -> Pstate -> Instr -> Pstate
moveinstr _ (Pstate pos dir ) (Turn c) = Pstate pos (turn dir c)
moveinstr p (Pstate pos dir ) (Move n) = Pstate (move p pos dir n) dir

move :: Path -> P -> P -> Int -> P 
move path pos dir n | n == 0 = pos 
                                | otherwise = maybe pos (\np -> move path np dir (n-1)) (next path pos dir)

next :: Path -> P -> P  -> Maybe P
next  (Path a rb cb) pos dir = if a A.! np' == ' ' then (select (wrap pos dir)) else select np'  
            where np' = tp pos dir 
                  select p = if a A.! p == '.' then Just p else Nothing
                  wrap (r,c) dir' = case dir' of 
                             (-1,0) -> (snd $ cb V.! c ,c)
                             (1,0) -> (fst $ cb V.! c ,c)
                             (0,1) -> (r, fst $ rb V.! r )
                             (0,-1) -> (r, snd $ rb V.! r )
                             _ -> error "undefined direction"
 
parseIntoArrayWithBorder  :: Char -> [[Char]] -> A.Array (Int, Int) Char
parseIntoArrayWithBorder c il = A.listArray bounds ((replicate cols c) ++ concat l ++ (replicate cols c) )
    where bounds = ((-1,-1),(rows -2 ,cols-2))
          l = map (\l' -> c:l'++[c]) $ il
          rows = length l + 2
          cols = maximum (map length il) + 2 

parsePath :: String -> Path
parsePath as =  Path a (V.fromList . map bnds $ l ) (V.fromList . map bnds . transpose $ l) 
      where 
         l = let cols = maximum (map length (lines as)) in  map (\l' -> l'++replicate (cols - length l') ' ' ) $ lines as 
         a = parseIntoArrayWithBorder ' ' l
         bnds xs =  let (a,b) = span (==' ') xs in (length a, length a -1 + length (takeWhile (/=' ') b))


getinitp :: Path -> Pstate
getinitp p@(Path a rb cb) = Pstate (fst . head . filter (\(p,v) -> v /= ' ') $ A.assocs a) (0,1)

password (Pstate (r,c) d) = (r+1)*1000+(c+1)*4+dirscore d 
         where dirscore = \case 
                     (0,1) -> 0
                     (0,-1) -> 2 
                     (1,0) -> 1
                     (-1,0) -> 3 
                     _ -> undefined

tpath = let [as,_] = splitOn "\n\n" test in parsePath as 
tinstr = let [_,s] = splitOn "\n\n" test in parseRow s 



--- part 2
moveinstr2 :: Int -> Path -> Pstate -> Instr -> Pstate
moveinstr2 _  _ (Pstate pos dir ) (Turn c) = Pstate pos (turn dir c)
moveinstr2 n path pstate (Move moves) = move2 n path pstate moves


move2 :: Int -> Path -> Pstate -> Int -> Pstate
move2 _ _ pstate 0 = pstate
move2 n p pstate moves = maybe pstate (\ps' -> move2 n p ps' (moves-1)) (next2 n p pstate )

next2 :: Int -> Path ->Pstate  -> Maybe Pstate
next2 n (Path a _ _ ) ps@(Pstate pos dir )  = if a A.! np' == ' ' then select (wrap2 n ps) else select (Pstate np' dir)  
            where np' = tp pos dir 
                  select (Pstate p dir' ) = if a A.! p == '.' then Just (Pstate p dir') else Nothing

zone n (r,c) = (r `div` n, c `div` n)
relzonecoords n (r,c) = (r `mod` n , c `mod` n)


wrap2 :: Int -> Pstate -> Pstate
wrap2 n  ps@(Pstate p d) =  addzone tz (wf (Pstate (relzonecoords n p) d))
            where (wf, tz) = wrapf n ps
                  addzone (zr,cz) (Pstate (r',c') d') = Pstate (zr*n+r', cz*n+c') d'


--    0,0    (0,1)  (0,2)
--    1,0    (1,1)   1,2 
--   (2,0)   (2,1)   2,2 
--   (3,0)    3,1    3,2

wrapf :: Int -> Pstate -> (Pstate -> Pstate, (Int, Int))
wrapf n p@(Pstate (r,c) dir) = 
   case (zone n (r,c), dir) of 
       ((0,1),(0,-1)) -> (flipx n . flipdir ,  (2,0)) -- 
       ((2,0),(0,-1)) -> (flipx n . flipdir , (0,1))  -- 
       --
       ((1,1),(0,-1)) -> (transp2 n . flipdir , (2,0)) --
       ((2,0),(-1,0)) -> (transp2 n . flipdir , (1,1)) -- 
       --
       ((0,2),(-1,0)) -> (flipx n . flipdir , (3,0)) --
       ((3,0),(1,0)) -> (flipx n .  flipdir, (0,2)) -- 
       -- 
       ((3,0),(0,-1)) -> (transp2 n . flipdir , (0,1))
       ((0,1),(-1,0)) -> (transp2 n . flipdir , (3,0))
       --
       ((3,0),(0,1)) ->  (transp2 n . flipdir , (2,1))
       ((2,1),(1,0)) ->  (transp2 n . flipdir , (3,0))
      -- 
       ((1,1),(0,1))  -> (transp2 n . flipdir ,  (0,2))
       ((0,2),(1,0)) ->  (transp2 n . flipdir ,  (1,1))
      -- 
       ((0,2),(0,1)) ->  (flipx n . flipdir , (2,1)) 
       ((2,1),(0,1)) ->  (flipx n . flipdir , (0,2)) 

       

      -- ((1,2),(0,1)) ->  (transp1 n . flipdir, (2,3))
      -- ((2,2),(1,0)) -> (flipy n . flipdir, (1,0))
      -- ((1,1),(-1,0)) -> (transp2 n . flipdir, (0,2))
       x -> error ("No transform defined for "++ show x )
      

-- transpose a relative Pstate along increasing axis
transp1 :: Int -> Pstate -> Pstate
transp1 n (Pstate (r,c) (dr,dc) ) = Pstate (n - c -1,n - r-1) (-dc, -dr) 
-- transpose a relative Pstate along decreasing axis
transp2 :: Int -> Pstate -> Pstate
transp2 n (Pstate (r,c) (dr,dc) ) = Pstate (c, r) (dc, dr) 
flipx n (Pstate (r,c) (dr,dc)) =  Pstate (n-r-1 , c ) (-dr, dc)
flipy n (Pstate (r,c) (dr,dc)) =  Pstate (r , n-c-1 ) (dr, -dc)
flipdir (Pstate (r,c) (dr,dc)) =  Pstate (r,c) (-dr,-dc)


part1 ::  String -> IO Int
part1 s = do
   let [as,ms] = splitOn "\n\n" s 
       path = parsePath as 
       instrs = parseRow ms 
       ip = getinitp path 

   -- putStrLn $ draw2dchararr (patha path )
   let fs =  foldl (moveinstr path) ip instrs 
   return $ password fs 

-- 7244
-- 27263 too low 
-- 117296 too low 
-- 145065
part2 ::  String -> IO Int
part2 s = do
  let [as,ms] = splitOn "\n\n" s 
      path = parsePath as 
      instrs = parseRow ms 
      ip = getinitp path
      n = 50 
  let fs =  foldl (moveinstr2 n path) ip instrs 
  return $ password fs


run :: IO ()
run = do
   putStrLn "--- Day22 Monkey Map ---"
   putStr " Part1: "
   readInp  "input22.txt" >>= part1 >>= assertIt 197160 
   putStr " Part2: "
   readInp  "input22.txt" >>= part2 >>= assertIt 145065


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 6032
     part2 test >>= assertIt 5031

test :: String
test = [r|        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5|]
