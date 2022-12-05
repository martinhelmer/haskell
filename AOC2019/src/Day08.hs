module Day08 where
import AOCHelper
import Data.List.Split
import Data.Char (digitToInt)
import Data.Function
import Data.List

rows = 6
columns = 25

countBy p = length . filter p

run :: IO ()
run = do
   putStrLn "Day08 ...."
   layers <- readInp "input08.txt" >>= \s -> return $ chunksOf (rows * columns) . map digitToInt $ head $ lines s
   putStr " Part1: " >> part1 layers >>= assertInt 1935
   putStr " Part2: " >> part2 layers


part1 :: [[Int]] -> IO Int
part1 layers = do
    let ms =  minimum $ map (\l -> (countBy (==0) l,l)) layers
    -- let ms = (0,minimumBy (compare `on` countBy (==0)) layers)
    return $ prod12 ms

prod12 (_,s) = countBy (==2) s * countBy (==1) s

-- part 2
part2 layers = do
    let img = foldl1 mergeLayers layers
    putStrLn ""
    putStr $ unlines $ chunksOf columns $ map intDispl img 
    makePbmFromInts columns rows img 


mergeLayers = zipWith (\ top bottom -> (if top == 2 then bottom else top))

-- !!
-- makeImage :: Layers -> Image
-- makeImage ls = map (head . dropWhile (==2)) $ transpose ls
