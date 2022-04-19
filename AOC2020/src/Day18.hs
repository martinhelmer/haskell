module Day18 where
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import Data.List

type Op = (Int -> Int -> Int)

opF '*' = (*)
opF '+' = (+)
opF _ = undefined

breakup :: String -> [(String,Char)]
breakup s@(x:xs) = case op of
                    ' ' -> [(lhs,' ')]
                    _ -> (lhs, op):breakup rhs
    where
        (op, rhs) = if rest == "" then (' ',"") else (rest !! 1 , drop 3 rest)
        (lhs, rest)  = if x == '(' then (sub, drop (length sub +1) xs)
                        else span isDigit s
        sub = getSub 0 "" xs
            where
                getSub :: Int -> String -> String -> String
                getSub _ _ [] = undefined
                getSub n acc (x:xs) | n == 0 && x == ')' = acc
                                    | n >= 0 = getSub fN (acc++[x]) xs
                                    | otherwise = undefined
                    where fN = case x of
                                ')' -> n-1
                                '(' -> n+1
                                _ -> n


eval :: String -> Int
eval s | all isDigit s = read s
       | otherwise = evalFold eval $ breakup s

eval' :: String -> Int
eval' s | all isDigit s = read s
       | otherwise = evalFold eval' doneWithPlus
        where doneWithPlus = foldr (\(s,op) l -> if op == '+' then (show (eval' s + eval' (fst . head $ l)),snd . head $ l):tail l else (s,op):l )  [] $ breakup s

evalFold :: (String -> Int) -> [(String, Char)] -> Int
evalFold evalF = fst . foldl'  (\(v, op) (ss, nextOp) -> (opF op v (evalF ss), nextOp))   (0, '+')

main = do
    l <- getContents
    print $ sum . map eval $ lines l
    print $ sum . map eval' $ lines l

