{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Day25 (run) where

import AOCHelper 
import Data.Bifunctor
import qualified Data.Map as M 
import Text.RawString.QQ ( r )

addSnafuDigit:: Char -> Char -> Char -> (Char,Char)
addSnafuDigit a b carry = bimap d2c d2c  ( l . sum . map  c2d $ [a,b,carry] )
         where  l s' | s' < 0 = bimap negate negate $ l (-s')  
                     | s' < 3 = (s', 0)
                     | otherwise = (s'-5 , 1)
                d2c d = M.fromList (zip ds cs) M.! d
                c2d d = M.fromList (zip cs ds) M.! d
                cs = "=-012"
                ds = [-2..2]::[Int]

addSnafu' :: [Char] -> [Char] -> [Char]
addSnafu' s1 s2 = rs (reverse s1) (reverse s2) '0'
        where rs [] [] carry = if carry == '0' then [] else [carry]
              rs [] ys carry = rs "0" ys carry
              rs xs [] carry = rs "0" xs carry
              rs (x:xs) (y:ys) carry = rs xs ys c++[a]
               where  (a,c) =  addSnafuDigit x y carry 
        
part1 :: String -> IO String
part1 s = do 
    return $ foldl1 addSnafu' (lines s) 

run :: IO ()
run = do
  putStrLn "--- Day25 SNAFU ---"
  putStr " Part1: "
  readInp  "input25.txt" >>= part1 >>= assertIt "2=000=22-0-102=-1001" 

runtest :: IO ()
runtest = do 
     part1 test1 >>= assertIt "2=-1=0"

test1 :: String
test1 = [r|1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122|]





