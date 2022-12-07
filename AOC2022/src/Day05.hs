module Day05 (run, runtest) where

import AOCHelper ( assertIt, readInp )
import Data.List (transpose)

--     [D]    
-- [N] [C]    
-- [Z] [M] [P]
--  1   2   3 

testInp :: [String]
testInp = ["NZ",
           "DCM",
           "P"]

testInstr :: [String]
testInstr = ["move 1 from 2 to 1",
            "move 3 from 1 to 3",
            "move 2 from 2 to 1",
            "move 1 from 1 to 2"]

runtest :: IO ()
runtest = do 
     part1 testInp testInstr >>= assertIt "CMZ"
     part2 testInp testInstr >>= assertIt "MCD"

-- -- test :: String
-- test = unlines ["[C]         [S] [H]",                
--                 "[F] [B]     [C] [S]     [W]",        
--                 "[B] [W]     [W] [M] [S] [B]",        
--                 "[L] [H] [G] [L] [P] [F] [Q]",        
--                 "[D] [P] [J] [F] [T] [G] [M] [T]",    
--                 "[P] [G] [B] [N] [L] [W] [P] [W] [R]",
--                 "[Z] [V] [W] [J] [J] [C] [T] [S] [C]",
--                 "[S] [N] [F] [G] [W] [B] [H] [F] [N]",
--                 " 1   2   3   4   5   6   7   8   9"]


-- parsing 
iToPos :: Num a => a -> a
iToPos n = n * 4 -3 

stripStuff :: [String] -> [String]
stripStuff xs = map q l
    where l = tail . reverse  $ xs
          q r = map (f r . iToPos) [1..9]
          f r pos = if pos > length r then ' ' else r !! pos

startList :: [String] -> [[Char]]
startList s =  map (reverse . filter (/= ' ')) $ transpose . stripStuff $ s

run :: IO ()
run = do
   putStrLn "--- Day05 Moving Crates ---"
   i <- readInp "input05.txt"
   let st = startList $ take 9 $ lines i
   let moves = drop 10 (lines i)
   putStr " Part1: "
   part1 st moves >>= assertIt "FWSHSPJWM"
   putStr " Part2: "
   part2 st moves >>= assertIt "PWPWHGFZS"

part1 :: [String] -> [String] -> IO String
part1 xs instr = do
    let q = foldl (onemove reverse) xs instr
    return $ map head q 
    
part2 :: [String] -> [String] -> IO String
part2 xs instr = do
    let q = foldl (onemove id) xs instr
    return $ map head q 

onemove :: ([a] -> [a]) -> [[a]] -> String -> [[a]]
onemove f boxes instr = onemove' f boxes n from to 
    where n = read (w !! 1)
          from = read (w !! 3) -1
          to = read (w !! 5) -1 
          w = words instr

-- ff is either 'reverse' or 'id' and defines the order we put the crates onto the 'to' stack / list
onemove' :: ([a] -> [a]) -> [[a]] -> Int -> Int -> Int -> [[a]]
onemove' ff boxes n from to = map f [0..(length boxes-1)]
    where f i | i == from = drop n (boxes !! i)
              | i == to = ff (take n (boxes !! from))++(boxes !! i)
              | otherwise = boxes !! i 
