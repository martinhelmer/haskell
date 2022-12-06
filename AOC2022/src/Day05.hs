module Day05 (run, runtest) where

import AOCHelper
-- import Data.List.Split (splitOn)

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

-- test :: String
-- test = unlines ["[C]         [S] [H]",                
--                 "[F] [B]     [C] [S]     [W]",        
--                 "[B] [W]     [W] [M] [S] [B]",        
--                 "[L] [H] [G] [L] [P] [F] [Q]",        
--                 "[D] [P] [J] [F] [T] [G] [M] [T]",    
--                 "[P] [G] [B] [N] [L] [W] [P] [W] [R]",
--                 "[Z] [V] [W] [J] [J] [C] [T] [S] [C]",
--                 "[S] [N] [F] [G] [W] [B] [H] [F] [N]",
--                 " 1   2   3   4   5   6   7   8   9"]

input :: [String]
input = ["CFBLDPZS",
        "BWHPGVN",
        "GJBWF",
        "SCWLFNJG",
        "HSMPTLJW",
        "SFGWCB",
        "WBQMPTH",
        "TWSF",
        "RCN"]

runtest :: IO ()
runtest = do 
     part1 testInp testInstr >>= assertIt "CMZ"
     part2 testInp testInstr >>= assertIt "MCD"


run :: IO ()
run = do
   putStrLn "--- Day05 ---"
   putStr " Part1: "
   i <- readInp "input05.txt"
   part1 input (drop 10 (lines i))>>= assertIt "FWSHSPJWM"
   putStr " Part2: "
   i <- readInp "input05.txt"
   part2 input (drop 10 (lines i))>>= assertIt "PWPWHGFZS"

part1 :: [String] -> [String] -> IO String
part1 xs instr = do
    let q = foldl (onemove reverse) xs instr
    return $ map head q 
    

onemove f boxes instr = onemove' f boxes n from to 
    where n = read (w !! 1)
          from = read (w !! 3) -1
          to = read (w !! 5) -1 
          w = words instr


onemove' ff boxes n from to = map f [0..(length boxes-1)]
    where f i | i == from = drop n (boxes !! i)
              | i == to = ff (take n (boxes !! from))++(boxes !! i)
              | otherwise = boxes !! i 

part2 :: [String] -> [String] -> IO String
part2 xs instr = do
    let q = foldl (onemove id) xs instr
    return $ map head q 