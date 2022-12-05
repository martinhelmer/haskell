module Day10 where

import AOCHelper
import Data.List
import Data.Ratio
import qualified Data.Set as S

type GridList = [Loc]
type Loc = (Int,Int)

run :: IO ()
run = do
   putStrLn "Day10 Asteroid Vaporizing!"
   gridList <- parseGrid <$> readInp "input10.txt"
   putStr " Part1: "
   laserLoc <- part1 gridList
   putStr " Part2: " >> part2 gridList laserLoc >>= assertInt  204

part1 :: GridList -> IO Loc
part1 l = do
    let calc = maximum . map (\t -> (numVisible l t,t)) $ l
    assertInt 296 $ fst calc
    return $ snd calc

part2 :: GridList -> Loc -> IO Int
part2 l loc = do
    let relPosList = tDiff loc l \\ [(0,0)]
        vaporizationOrder = sortOn (degs relPosList) relPosList
    return $ (\ (x, y) -> 100 * x + y) . tOp (+) loc $ vaporizationOrder !! 199

parseGrid :: String -> [Loc]
parseGrid s = concat $ zipWith zipLine [0..] (lines s)
    where zipLine lineNo line = map (\ q -> (fst q , lineNo)) . filter (\ (_, a) -> a == '#')  $ zip [0 .. ] line

tDiff t = map (tOp (flip (-)) t)

tOp op (a1,b1) (a2,b2) = (a1 `op` a2, b1 `op` b2)

inBetweens :: Integral b => (b, b) -> (b, b) -> [(b, b)]
inBetweens t1 t2 | t1 == t2 = []
                 | otherwise = tail . takeWhile (/=t2) $ iterate (tOp (+) stepSize ) t1
    where stepSize | x' == 0 = (0,sign y')
                   | y' == 0 = (sign x',0)
                   | otherwise = (sign x' * numerator p,sign y' * denominator p)
          (x',y') = tOp (-) t2 t1
          p = abs x' % abs y'

canSee s t1 t2 | t1 == t2 = False
               | otherwise = all ( `notElem` s) $ inBetweens t1 t2

numVisible s t = length . filter id $ map (canSee s t) s

numBetween s t1 t2 = length . filter (`elem` s) $ inBetweens t1 t2

angle :: Int -> Int -> Float
angle x y = let q = 180 / pi * atan2 (fromIntegral x) (fromIntegral (-y))
            in if q < 0 then 360 + q else q

degs :: Foldable t => t Loc -> Loc -> Float
degs s tup = fromIntegral ( 360 * numBetween s (0,0) tup) + uncurry angle tup