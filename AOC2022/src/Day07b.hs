{-# LANGUAGE QuasiQuotes #-}
module Day07b (run, runtest) where

import AOCHelper
import Text.RawString.QQ
import Data.List (isPrefixOf) 
import Data.Char

test :: String
test = [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|]

runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 95437
     part2 test >>= assertIt 24933642


isDown :: [Char] -> Bool
isDown x = isPrefixOf "$ cd" x && not (isUp x)

isUp :: [Char] -> Bool
isUp = isPrefixOf "$ cd .." 

isFile :: String -> Bool
isFile = isDigit . head  

fileSize :: String -> Int
fileSize = read . head . words

addAt :: Int -> Int -> [Int] -> [Int]
addAt n x xs = take n' xs ++ (head t +x):tail t
    where n' = (length xs - n) - 1
          t = drop n' xs 



go :: Int -> [Int] -> [Int] -> [[Char]] -> [Int]
go _ [] sums [] = sums 
go pos hist sums l  | null l = doUp []
                    | isDown x = go (length sums) (pos:hist) (0:sums) xs
                    | isFile x = go pos hist  (addAt pos (fileSize x) sums) xs
                    | isUp x = doUp xs
                    | otherwise = go pos hist sums xs 
            where x = head l
                  xs = tail l
                  doUp = go (head hist) (tail hist) (addAt (head hist) (sums !! (length sums - pos -1)) sums) 

dirsizes :: String -> [Int]
dirsizes s = go 0 [] [0] (lines s) 
-- 0 [] [0] 
-- 0 [] [1]
-- 1 [0] [0:1]
-- 1 [0] [3:1]
-- 0 [] [3:4]
-- 
part1 :: String -> IO Int
part1 s = do
    return $ sum . filter (< 100000)  $  dirsizes s
    
spaceNeeded :: Num a => [a] -> a
spaceNeeded xs = 30000000 - (70000000 - last xs )

part2 :: String -> IO Int
part2 s = do
    let ds = dirsizes s 
    return $ minimum . filter (>= spaceNeeded ds) $ ds
    
run :: IO ()
run = do
   putStrLn "--- Day07 DirTree with stack ---"
   putStr " Part1: "
   readInp "input07.txt" >>= part1 >>= assertIt 1513699
   putStr " Part2: "
   readInp "input07.txt" >>= part2 >>= assertIt 7991939



