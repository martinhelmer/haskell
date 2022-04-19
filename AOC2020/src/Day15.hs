module Day15 where 
    
import Data.Maybe
import Data.List
import qualified Data.Map as Map

nextWord :: [Int] -> Int
nextWord (x:xs) = maybe 0 (1 +) i
    where i = elemIndex x xs

grow l = nextWord l:l

-- | Part 2 
type LastWord = (Int,Int) -- (word, pos) 
type History = Map.Map 
data WordGame = WordGame LastWord (History Int Int) deriving Show


nextWord2 :: WordGame -> Int
nextWord2 (WordGame (word, pos) m) = maybe 0 (pos - ) $ Map.lookup word m

addWord :: WordGame -> Int -> WordGame
addWord (WordGame (word, pos) m) newWord = WordGame (newWord, pos+1) (Map.insert word pos m)

grow2 :: WordGame -> WordGame
grow2 g = addWord g $ nextWord2 g 

getLast (WordGame l g) = l

initGame :: [Int] -> WordGame
initGame l = WordGame (last l, length l) (Map.fromList $ zip (init l) [1..])
main = do
    print $ head $ iterate grow (reverse [0,1,4,13,15,12,16]) !! (2020-7)
    print $ getLast $ iterate grow2 ( initGame [0,1,4,13,15,12,16]) !! (30000000-7)
