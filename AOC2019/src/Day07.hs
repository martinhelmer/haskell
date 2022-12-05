module Day07 where

import qualified Data.Vector.Unboxed as V
import           Data.List
import           Data.Maybe
import AOCHelper
import IntCode

t1 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
t2 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
test1 = last $ takeWhile ((/=) Nothing . fst) $ iterate  doOnce (Just 0, getInitialAmps (parseInp t1) [9,8,7,6,5])

test2 = last $ takeWhile ((/=) Nothing . fst) $ iterate  doOnce (Just 0, getInitialAmps (parseInp t2) [9,7,8,5,6])


run :: IO ()
run = do
   putStrLn "Day07 ..."
   putStr " Part1: " >> readInp "input07.txt" >>= part1 >>= assertInt 117312
   putStr " Part2: " >> readInp "input07.txt" >>= part2 >>= assertInt 1336480

-- part1 
part1 :: String -> IO Int
part1 s = do
    let v = parseInp s
    let r =  maximum . map (foldl (\a b -> head $ output (runProg v [b,a])) 0) $ permutations [0..4]
    return r

-- part12
part2 :: String -> IO Int
part2 s = do
    let v = parseInp s 
    let r = maximum . map (getThrust v) $  permutations [5..9]
    return r 

-----
parseInp :: String -> V.Vector Int
parseInp s = V.fromList (read $ "[" ++ s ++ "]")

getInitialAmps :: V.Vector Int -> [Int] -> [PComputer]
getInitialAmps v = map (\i -> runProg v [i])

doOnce :: (Maybe Int , [PComputer]) -> (Maybe Int , [PComputer])
doOnce (Nothing,x:xs) = (Nothing,xs++[x])
doOnce (_,[]) = undefined
doOnce (Just i,x:xs) = (head' $ output c, xs++[clearOut c])
    where c = runComp $ addInp x i

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

getThrust v l = fromJust . last $ takeWhile (/= Nothing)  $ map fst $ iterate doOnce (Just 0, getInitialAmps v l)
