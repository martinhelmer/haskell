{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Chipher where

import Data.Char as Ch

-- ciphering
alphaRange = ord 'z' - ord 'a' + 1

cipherChar :: Int -> Char -> Char
cipherChar offset c | c `elem` ['a'..'z'] = offChr 'a'
                | c `elem` ['A'..'Z'] = offChr 'A'
                | otherwise = c
    where offChr base = chr $ mod (ord c - ord base + offset) alphaRange + ord base

myChipher :: [Int] -> String -> String
myChipher ol = zipWith cipherChar (cycle ol)

myUnchipher :: [Int] -> String -> String
myUnchipher ol = myChipher (map negate ol)

-- writing my own

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []  = []
squishMap f (x:xs) = f x ++ squishMap f xs 

squishMapAgain  :: [[a]] -> [a]
squishMapAgain = squishMap id