module Day03 where

import           AOCHelper
import qualified Data.Vector as V
import           Data.Maybe
import qualified Data.List.Split as S
import           Data.List
import qualified Data.Set as Set
import Data.Function (on)

test1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
test2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
run :: IO ()
run = do
   putStrLn "Day03 : -- Crossed Wires -- "
   putStr " Part1: " >> readInp "input03.txt" >>= part1
   putStr " Part2: " >> readInp "input03.txt" >>= part2

-- part1 - 709
part1 :: String -> IO()
part1 s =  do
    let (l1, l2) = walks s
    print $ Set.findMin $ Set.map mDist $ i l1 l2

-- part2 - 13834
part2 :: String -> IO()
part2 s =  do
    let (l1,l2) = walks s
    print $ Set.findMin $ Set.map (sumDistToP (l1,l2) ) $ i l1 l2

i l1 l2 =  Set.intersection (Set.fromList l1) (Set.fromList l2)

mDist (a,b) = abs a + abs b

walks s = (walk $ S.splitOn "," s1, walk $ S.splitOn "," s2)
    where [s1,s2] = lines s

w :: (Num a) => [Char] -> [(a, a)]
w [] = undefined
w (x:xs) = replicate (read xs) d
    where d = case x of
                'R' -> (1,0)
                'L' -> (-1,0)
                'U' -> (0,1)
                'D' -> (0,-1)
                _ -> undefined

tsum :: (Num a) => (a, a) -> (a, a) -> (a, a)
tsum (a,b) (c,d) = (a+c,b+d)

walk :: (Num a, Foldable t) => t [Char] -> [(a, a)]
walk s = tail $ scanl tsum (0,0) $ concatMap w s

distToP p l  = fromJust $ elemIndex p l

sumDistToP :: Eq a => ([a], [a]) -> a -> Int
sumDistToP (l1,l2) p =sum . map (distToP p) $ [l1,l2]
