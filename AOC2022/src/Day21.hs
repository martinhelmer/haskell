{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module Day21 (run) where

import Data.List ( foldl' ) 
import AOCHelper ( assertIt, readInp)
import Data.Maybe ( fromJust, mapMaybe, isJust )
import qualified Data.Map as M
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))
import Text.ParserCombinators.ReadP
import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( isDigit ) 
import Text.Read (readMaybe)
-- Parse 


op :: ReadP (Double -> Double -> Double)
op = (\case 
        '-' -> (-) 
        '*' -> (*)
        '/' -> (/) 
        '+' -> (+)
        c -> error ("unexpected char:"++[c]) ) <$> get


spaces = munch (== ' ')

number :: ReadP Double
number = do
   c <-  satisfy isDigit
   b <-  munch (\c -> isDigit c)
   return $ read (c:b) 
   

word :: ReadP String
word = munch (/= ' ')

rest :: ReadP String
rest = munch (const True)

parseMonkey :: ReadP Monkey
parseMonkey = do
   name  <- munch (/=':')
   _ <- string ": " 
   (Op name <$> word <*  spaces <*> op <* spaces <*> word) <|> (Number name <$> number)

--  (Number name <$> number) <|> 

parseRow :: String -> Monkey
parseRow = fst . head . readP_to_S parseMonkey

type MonkeyName = String 
data Monkey = Number {mid ::MonkeyName, mval::Double} | Op {mid::MonkeyName, mleft::MonkeyName, mop::(Double -> Double -> Double), mright::MonkeyName} 
type MonkeyMap = M.Map MonkeyName Monkey
-- 

instance Show Monkey where  
  show (Number n i ) = show n ++ show i 
  show (Op n o n2 n3) = show n ++ show o ++ show n3 

eval :: MonkeyMap -> Monkey -> Double
eval mm (Number mid i ) = i 
eval mm (Op mid ml o mr) = o (eval mm (fromJust $ M.lookup ml mm)) (eval mm (fromJust $ M.lookup mr mm))


part1 ::  String -> IO Int
part1 s = do
    let mm = M.fromList $ map (\s' -> let m = parseRow s' in (mid m, m)) (lines s) 
    print $ eval mm  (fromJust $ M.lookup "root" mm)
    return 0

calcroot mm = (eval mm (fromJust $ M.lookup (mleft root) mm), eval mm (fromJust $ M.lookup (mright root) mm))
    where root = fromJust $ M.lookup "root" mm

-- 3952673930914
part2 ::  String -> IO Int
part2 s = do
  putStrLn ""
  let mm = M.fromList $ map (\s' -> let m = parseRow s' in (mid m, m)) (lines s) 
      l = map (\i -> calcroot (M.insert "humn" (Number "humn" i) mm)) 
         [301, 3900000000000,
          3940000000000,
          3952673930912]
  putStrLn $ concat $ map (\(l,r) -> show (l,r,r-l) ++ "\n") l
  return 0


run :: IO ()
run = do
   putStrLn "--- Day21 Monkeys! ---"
   putStr " Part1: "
   readInp  "input21.txt" >>= part1 >>= assertIt 54703080378102 
   putStr " Part2: "
   readInp  "input21.txt" >>= part2 >>= assertIt 15960


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 152
     part2 test >>= assertIt 3348

test = unlines ["root: pppw + sjmn",
  "dbpl: 5",
  "cczh: sllz + lgvd",
  "zczc: 2",
  "ptdq: humn - dvpt",
  "dvpt: 3",
  "lfqf: 4",
  "humn: 5",
  "ljgn: 2",
  "sjmn: drzm * dbpl",
  "sllz: 4",
  "pppw: cczh / lfqf",
  "lgvd: ljgn * ptdq",
  "drzm: hmdt - zczc",
  "hmdt: 32"]