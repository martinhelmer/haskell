module Day16 where
import AOCHelper
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed as UV


type MyInt = Int

bp :: [MyInt]
bp = [0,1,0,-1]

allPatterns :: [[MyInt]]
allPatterns = [tail . concat . repeat . concatMap (replicate i ) $ bp | i <- [1..]]


allp num = take num allPatterns

t1 :: [Int]
t1 = map (\x -> read [x]::Int) "80871224585914546619083218645595"

phase :: [MyInt] -> [MyInt]
phase nums =  map (\x -> (abs . sum . zipWith (*) nums $ x) `mod` 10 ) (take (length nums) allPatterns)

phasev :: [[Int]] -> [Int] -> [Int]
phasev ap nums =  map (\x -> (abs . sum . zipWith (*) nums $ x) `mod` 10 ) ap 

run :: IO ()
run = do
   putStrLn "--- Day16 ---"
   putStr " Part1: "
   readInp "input16.txt" >>= part1 >>= assertInt 74369033
   putStr " Part2: "
   readInp "input16.txt" >>= part2 >>= assertInt 19903864

part1 s = do
    let v = parseString s :: [MyInt]
    return $ read $ concatMap show $ take 8 $ iterate phase v !! 100

part1b s = do
    let v = parseString s :: [MyInt]
        allp' = allp (length v)
    return $ read $ concatMap show $ take 8 $ iterate (phasev allp') v !! 100

-- initial solution
part2b s = do
    let v = concat . replicate 10000 $ parseString s :: [MyInt]
        fromIx = read ( take 7 s) :: Int
        v2 = drop fromIx v
        s2 = concatMap show $ take 8 $ iterate i2 v2 !! 100
    return (read s2 :: Int )


-- use scanl
part2c s = do -- 59767332 too high
    let v = concat . replicate 10000 $ parseString s :: [MyInt]
        fromIx = read ( take 7 s) :: Int
        v2 = reverse $  drop fromIx v
        s2 = concatMap show $ take 8 $ reverse $ iterate (scanl1 (\a b -> (a+b) `mod` 10 )) v2 !! 100
    return (read s2 :: Int )

i2 l  = foldr (\a b -> ((head b + a ) `mod` 10):b)  [last l] (init l)

part2 s = do 
    let v = concat . replicate 10000 $ parseString s :: [MyInt]
        fromIx = read ( take 7 s) :: Int
        v2 =  UV.fromList (drop fromIx v)
        s2 = concatMap show $ UV.toList $ UV.take 8 $ iterate (UV.scanr1 (\a b -> (a+b) `mod` 10 )) v2 !! 100
    return (read s2 :: Int )

