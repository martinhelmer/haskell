module Day06 where

import           Data.Maybe
import           Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import           AOCHelper

testS = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
test2 =  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

s2t s = (,) a b
    where [b,a] = splitOn ")" s

path :: M.Map [Char] [Char] -> [Char] -> [[Char]]
path m s = takeWhile (/= "COM") $ iterate (\s -> fromJust $ M.lookup s m) s

numOrbs m s = length $ path m s

transfers m a b = length (S.union s1 s2 `S.difference` S.intersection s1 s2) - 2 
    where s1 = S.fromList $ path m a
          s2 = S.fromList $ path m b
run :: IO ()
run = do
   putStrLn "Day06 ..."
   putStr "Part1: " >> readInp "input06.txt" >>= part1 >>= assertInt 223251
   putStr "Part2: " >> readInp "input06.txt" >>= part2 >>= assertInt 430 

-- part1 
part1 :: String -> IO Int
part1 s =  do
   let  m = M.fromList $ map s2t $ lines s
   return $ sum . map (numOrbs m) $ M.keys m

 -- part2 430<, 431, 432,  <444
part2 :: String -> IO Int
part2 s =  do 
     let  m = M.fromList $ map s2t $ lines s
     return $ transfers m "YOU" "SAN"
