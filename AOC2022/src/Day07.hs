{-# LANGUAGE QuasiQuotes #-}
module Day07 (run, runtest) where

import AOCHelper
import Text.RawString.QQ
import Data.List ( foldl', isPrefixOf, tails ) 
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

subdirs xs =  map tail $ filter (isDown . head) . filter (not . null) $ tails xs

dirSize :: Int -> Int -> [[Char]] -> Int
dirSize _ cum [] = cum
dirSize d cum (x:xs)  | d < 0 = cum
                       | isDown x = dirSize (d+1) cum xs
                       | isUp x = dirSize (d-1) cum xs
                       | isFile x = dirSize d (cum + (fileSize x)) xs
                       | otherwise = dirSize d cum xs

dirsizes :: String -> [Int]
dirsizes s = map (dirSize 0 0 ) $ subdirs $ lines s


spaceNeeded :: Num a => [a] -> a
spaceNeeded xs = 30000000 - (70000000 - (head xs) )

part1 :: String -> IO Int
part1 s = do
    return $ sum . filter (< 100000)  $  dirsizes s
    
part2 :: String -> IO Int
part2 s = do
    let ds = dirsizes s 
    return $ minimum . filter (>= spaceNeeded ds) $ ds
    
run :: IO ()
run = do
   putStrLn "--- Day07 DirTree ---"
   putStr " Part1: "
   readInp "input07.txt" >>= part1 >>= assertIt 1513699
   putStr " Part2: "
   readInp "input07.txt" >>= part2 >>= assertIt 7991939



