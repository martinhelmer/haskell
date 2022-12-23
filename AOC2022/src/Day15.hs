{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day15 (run) where

import Text.ParserCombinators.ReadP
import AOCHelper ( assertIt, readInp)
import Data.Char (isDigit)
import Text.RawString.QQ ( r )
import Data.List ( sort ) 
import Data.Containers.ListUtils ( nubOrd )
import Data.Maybe ( mapMaybe ) 

type P = (Int,Int)

-- n = row to intesrect
-- p = position to intersect from
-- r = radius that p sensor covers
-- -> (p1, p2) = (segment of row intersected)
intersectionOf :: (Ord b, Num b) => b -> (b, b) -> b -> Maybe (b, b)
intersectionOf n p r' = if rem' >= 0 then Just (minp,maxp) else Nothing 
  where minp = px - rem'
        maxp = px + rem' 
        (px,py) = p 
        rem' = r' - abs (py-n)

intersectionsOf :: (Ord b, Num b) => b -> [((b, b), b)] -> [(b, b)]
intersectionsOf n = mapMaybe (uncurry (intersectionOf n))

md :: P -> P -> Int 
md (x,y) (x2,y2) = abs (x - x2) + abs (y - y2)

number :: ReadP Int
number = read  <$> munch (\c -> isDigit c || c =='-')

rParse :: ReadP ((Int,Int),(Int,Int))
rParse = do
   _  <- string "Sensor at x="
   sx <- number 
   _ <- string ", y="
   sy <- number 
   _ <- string ": closest beacon is at x="
   bx <- number 
   _ <- string ", y="
   by <- number
   return ((sx,sy),(bx,by))

parseRow :: String -> ((Int, Int), (Int, Int))
parseRow = fst . head . readP_to_S rParse


mergeRange :: [(Int,Int)] -> [(Int,Int)]
mergeRange l = foldl go [head sl] $ tail sl 
  where sl = sort l 
        go acc@((a1,b1):xs) (a2,b2) | a2 > (b1+1) = (a2,b2):acc 
                                    | otherwise = (a1, max b1 b2):xs

beaconsOnRow :: Int -> [(P,P)] -> [Int]
beaconsOnRow n l = nubOrd .  map fst $ filter (\(_,y) -> y == n) $  map snd l

beaconsInRange :: Ord a => [a] -> (a, a) -> Int
beaconsInRange bs (x1,x2) = length . filter (\b -> b >= x1 && b <= x2) $ bs 

part1 :: Int -> String  -> IO Int
part1 n s  = do  
  let points = map parseRow $ lines s 
      distances = map (\(s',b) -> (s', md s' b )) points 
      intsct =  intersectionsOf n $ distances
      bl = beaconsOnRow n points 
  -- print (distances)
  return $ sum . map (\(a,b) -> b-a+1 - beaconsInRange bl (a,b)) $ mergeRange intsct 


checkRow :: [((Int, Int), Int)] -> Int -> Int -> Maybe Int
checkRow distances lim n | fst (head mr) <= 0 && snd (head mr) >= lim = Nothing 
                         | fst (head mr) > 0 = Just n 
                         | length mr == 1 = Just (4000000 * lim + n)
                         | otherwise = Just (4000000 * (snd (head mr) + 1)  + n) 
      where mr = sort $  mergeRange (intersectionsOf n distances)

-- 4895132 too low 
part2 :: Int ->  String -> IO Int
part2 n s = do  
  let points = map parseRow $ lines s 
      distances = map (\(s',b) -> (s', md s' b )) points 
  return (head . mapMaybe (checkRow distances n) $ [0..n])

run :: IO ()
run = do
   putStrLn "--- Day15 ---"
   putStr " Part1: "
   readInp  "input15.txt" >>= part1 2000000 >>= assertIt 4876693
   putStr " Part2: "
   readInp  "input15.txt" >>= part2 4000000 >>= assertIt 11645454855041

testR :: String
testR = "Sensor at x=2810797, y=2300748: closest beacon is at x=2946873, y=2167634"

runtest :: IO ()
runtest = do 
     part1 10 test >>= assertIt 26
     part2 20 test >>= assertIt 56000011


test :: String
test = [r|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
|]