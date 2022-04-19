module Day23b where 

import System.TimeIt
import qualified Data.Vector as V
import Data.Int 

startState = parseInp 389125467
startState2 = parseInp 389125467 ++ [10..10000]

parseInp :: Show a => a -> [Int]
parseInp i = read ("[" ++ go (show i) ++ "]")
    where go (x:[]) = x:[]
          go (x:xs) = x:',':go xs


parseInp2 :: Show a => a -> [TheT]
parseInp2 i = read ("[" ++ go (show i) ++ "]")
    where go (x:[]) = x:[]
          go (x:xs) = x:',':go xs

nextState :: [Int] -> [Int]
nextState (current:x:y:z:xs) = front++[d,x,y,z]++back++[current]
    where dest = if not (null fl) then maximum fl  else maximum xs
          (front,d:back) = span (/=dest) xs
          fl = filter (<current) xs

nextState2 :: Int -> [Int] -> [Int]
nextState2 m (current:x:y:z:xs) = front++[d,x,y,z]++back++[current]
    where dest 1 | m `notElem`  pick = m
                 | (m-1) `notElem` pick = m-1
                 | (m-2) `notElem` pick = m-2
                 | (m-3) `notElem` pick = m-3
                 | otherwise = m-4
          dest 2 | 1 `notElem` pick = 1
                 | otherwise = dest 1
          dest 3 | 2 `notElem` pick = 2
                 | otherwise = dest 2
          dest 4 | 3 `notElem` pick = 3
                 | otherwise = dest 3
          dest n | (n-1) `notElem` pick = n-1
                 | (n-2) `notElem` pick = n-2
                 | (n-3) `notElem` pick = n-3
                 | otherwise = n-4
          pick = [x,y,z]
          (front,d:back) = span (/=(dest current)) xs



doNtimes f 0 v = v
doNtimes f n v = doNtimes f (n-1) (f v)

-- part 2 
type TheT = Int32
type TheV = V.Vector TheT

getStartVector :: Show a => a -> TheT -> TheV
getStartVector i s = V.fromList [(1::TheT)..(s+1::TheT)] V.//  ((fromIntegral s,head pi):(0,head pi):zip (map fromIntegral pi) (tail pi ++ [fromIntegral(1+length pi)]))
                     where pi = parseInp2 i 

pick :: TheV -> [Int]
pick v = take 3 $ iterate (fromIntegral . (v V.!)) (fromIntegral $ v V.! (current v))

current :: TheV -> Int
current v = fromIntegral $ v V.! 0

nextCurr ::TheV -> Int
nextCurr v = fromIntegral (v V.! (last $ pick v))


fromCicrular :: TheV -> [Int]
fromCicrular v = stop:nexts (fromIntegral(v V.! stop)) v 
       where stop =  fromIntegral ( v V.! 0)
             nexts :: Int ->  TheV -> [Int]
             nexts i v | i == stop = []
                       | otherwise = i : nexts (fromIntegral $ v V.! i) v     

dest :: Int -> [Int] -> Int -> Int
dest m pick 1 | m `notElem`  pick = m
              | (m-1) `notElem` pick = m-1
              | (m-2) `notElem` pick = m-2
              | (m-3) `notElem` pick = m-3
              | otherwise = m-4
dest m pick 2 | 1 `notElem` pick = 1
              | otherwise = dest m pick 1
dest m pick 3 | 2 `notElem` pick = 2
              | otherwise = dest m pick 2
dest m pick 4 | 3 `notElem` pick = 3
              | otherwise = dest m pick 3
dest m pick n | (n-1) `notElem` pick = n-1
              | (n-2) `notElem` pick = n-2
              | (n-3) `notElem` pick = n-3
              | otherwise = n-4

nextState3 :: TheV -> TheV
nextState3 v = v V.// [(0,fromIntegral nc),(curr,fromIntegral nc), (fromIntegral d,fromIntegral $ head pck), (last pck, v V.! d)]    
       where m = (length v) - 1
             nc =  nextCurr v
             pck = pick v 
             curr = fromIntegral $ current v
             d = dest m pck curr

main :: IO ()
main = do
--     timeIt $ print $ take 5 $ (doNtimes (nextState) 100 startState)   -- naive 
--     timeIt $ print $ take 5 $ (doNtimes (nextState) 1000 startState2) -- naive 
--     timeIt $ print $ take 5 $ (doNtimes (nextState2 10000) 1000 startState2)   -- naive 
    timeIt $ print $ take 5 $ fromCicrular $ (doNtimes (nextState3) 1000 (getStartVector 389125467 1000000))   -- naive 
    