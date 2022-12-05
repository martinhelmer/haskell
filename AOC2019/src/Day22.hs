module Day22 where
import Data.List
import GHC.RTS.Flags (GCFlags(doIdleGC))
import AOCHelper

data Op = Rev | Cut | Incr  deriving Show
data Instr = Instr Op Int deriving Show

test5 = unlines ["deal into new stack",
        "cut -2",
        "deal with increment 7",
        "cut 8",
        "cut -4",
        "deal with increment 7",
        "cut 3",
        "deal with increment 9",
        "deal with increment 3",
        "cut -1"]
test5r =    "Result: 9 2 5 8 1 4 7 0 3 6"

cut n l =  b ++ a
    where (a,b) = splitAt v l
          v = if n > 0 then n else length l + n

newStack = reverse

increment n l = map snd . sort $ increment' n l (length l) 0
increment' _ [] _ _ = []
increment' n (x:xs) maxl cp = (cp,x):increment' n xs maxl ((cp+n) `mod` maxl)

parseRow s | take 9 s == "deal into"  = Instr Rev 0
           | take 20 s == "deal with increment " = Instr Incr (read (drop 20 s))
           | take 3 s == "cut" = Instr Cut (read (drop 4 s))
           | otherwise = undefined

doInstr l i = case i of
            Instr Rev _ -> newStack l
            Instr Cut n -> cut n l
            Instr Incr n -> increment n l

part1 :: (Ord b, Num b, Enum b) => String -> [b]
part1 s = do -- 1510! 
    foldl doInstr [0..10006] $ map parseRow $ lines s 