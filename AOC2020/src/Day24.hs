module Day24 where

import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Data.Map as Map
import Data.List.NonEmpty (groupWith)
import Data.Bifunctor

eol :: Parser Char
eol = char '\n'

p =  many (many1 expr <* many eol)

expr :: Parser String
expr = try (string "se") <|> string "e" <|> string "sw" <|> string "w" <|> try (string "ne") <|> string "nw"

type HexPos = (Int,Int)
type HexMap = Map.Map HexPos Bool

startHex = (0,0)

move :: HexPos -> String -> HexPos
move (x,y) s = case s of
                    "w" -> (,) (x-1) y
                    "e" -> (,) (x+1) y
                    "sw" -> (,) x (y-1)
                    "se" -> (,) (x+1) (y-1)
                    "ne" -> (,) x (y+1)
                    "nw" -> (,) (x-1) (y+1)
                    _ -> undefined

walk :: HexPos -> [String] ->HexPos
walk = foldl move

initialMap l = Map.filter id $ foldr (\p l -> Map.insertWith (\x _ -> not x) p True l)  Map.empty tiles
    where tiles = map (walk startHex) $ fromRight [] ( parse p "" l )

part1 l = Map.size (initialMap l)


-- part 2 
part2 l = Map.size $ growNTimes 200 (initialMap l)

neighbors :: HexPos -> [HexPos]
neighbors (x,y) = map (Data.Bifunctor.bimap (x +) (y +)) [(-1,0),(1,0),(0,-1),(1,-1),(0,1),(-1,1)]

blackNeighbors :: HexMap -> Map.Map HexPos Int
blackNeighbors m =  Map.fromListWith (+) $ zip (concatMap neighbors (Map.keys m)) (repeat 1)


growNTimes :: Int -> HexMap -> HexMap
growNTimes 0 m = m
growNTimes n m = growNTimes (n-1) growOnce
    where growOnce = Map.map (const True) $ Map.filterWithKey (\k a -> a == 2 || (a == 1 && Map.member k m)) $ blackNeighbors m

--
runner :: IO ()
runner = do
    l <- getContents
    putStrLn $ "Part 1:" ++ show (part1 l)
    putStrLn $ "Part 2:" ++ show (part2 l)


