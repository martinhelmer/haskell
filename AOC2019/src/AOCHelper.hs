{-# LANGUAGE FlexibleInstances #-}

module AOCHelper where
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as M
import qualified Data.Array as A

import Data.Maybe
import           Data.List.Split
import           Data.Int

import Data.Foldable
import Data.Function (on)

mapInsert:: (Ord a) => M.Map a b -> [(a,b)] -> M.Map a b
mapInsert = foldl (\r (a,b) -> M.insert a b r)


readInp :: [Char] -> IO String
readInp = readFile . (++) "input/"


assertInt :: Int -> Int -> IO()
assertInt  expected result | result == expected = putStrLn $ show result ++ " - OK"
                           | otherwise  = putStrLn $ show result ++" - Fail! expected "++show expected

printreturn :: (Show a) => a -> IO a
printreturn x = putStr (show x) >> return x

makePbmFromInts :: Int -> Int -> [Int] -> IO ()
makePbmFromInts w h im = writeFile "./message.pbm" pbm
  where pbm = "P1 " ++ show w ++ " " ++ show h ++ " " ++ unwords (map show im)

sign i = if i < 0 then -1 else 1

class Parseable a where
    parseString :: String -> a

instance Parseable (V.Vector Int) where
    parseString s = V.fromList (read $ "[" ++ s ++ "]")

instance Parseable (UV.Vector Int) where
    parseString s = UV.fromList (read $ "[" ++ s ++ "]")

instance Parseable [Int16] where
    parseString s  = map (\x -> read [x]::Int16) (init s)

instance Parseable [Int8] where
    parseString s  = map (\x -> read [x]::Int8) (init s)

instance Parseable [Int] where
    parseString s  = map (\x -> read [x]::Int) (init s)

parseIntoArray :: [Char] -> A.Array (Int, Int) Char
parseIntoArray s = A.listArray bounds (concat l)
    where bounds = ((0,0),(length l -1,length (head l) -1))
          l = lines s



parseInto2dMap s = M.fromList $ zip ([(x,y) | y <- [0..(rows-1)] , x <-[0..(cols-1)]]) (concat lns)
    where lns  = lines s
          rows = length lns
          cols = length . head $ lns



mapBounds a = ((x1,y1),(x2,y2))
    where x1 =  minimum . map fst $ a
          x2 =  maximum . map fst $ a
          y1 =  minimum . map snd $ a
          y2 =  maximum . map snd $ a


draw2dmap m = unlines $ chunksOf (1+x2-x1) (map (\k -> intDispl $ fromMaybe 0 (M.lookup k m)) ( flip (,) <$>  [y1..y2] <*> [x1..x2]))
    where  ((x1,y1),(x2,y2)) = mapBounds (M.keys m)

draw2dcharmap m = map (\c -> if c =='#' then '\x2588' else c) $ unlines $ chunksOf (1+x2-x1) (map (\k -> fromMaybe ' ' (M.lookup k m)) ( flip (,) <$>  [y1..y2] <*> [x1..x2]))
    where  ((x1,y1),(x2,y2)) = mapBounds (M.keys m)


draw2dchararr a = draw2dcharmap ( M.fromList $ A.assocs a)

intDispl 1 = '\x2588'
intDispl 0 = '.'
intDispl 2 = '\x2592'
intDispl 3 = '='
intDispl 4 = '\x25CD'

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)
