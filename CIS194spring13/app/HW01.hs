module Main where

import Hanoi


main :: IO ()
main = do
    putStrLn $ "hanoi 3 a b c = " ++ show (hanoi 3 "a" "b" "c")
    putStrLn $ "hanoi 15 a b c = " ++ show (length $ hanoi 15 "a" "b" "c")
