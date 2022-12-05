module Day25 where

import System.TimeIt
import Data.List
import GHC.RTS.Flags (GCFlags(stkChunkBufferSize))
k1 = 5293040
k2 = 8184785


myit f n v | n == 0 = v
           | otherwise = myit f (n-1) (f v)

-- k1 = 5764801
-- k2 = 17807724

doLoop s n = n * s `rem` 20201227

findSize' s key =  length . takeWhile (/=key) . iterate (doLoop s) $ 1

findSize :: Int -> Int -> Int -> Int -> Int
findSize subject lookfor count current | current == lookfor = count
                                       | otherwise = findSize subject lookfor (count+1) (current * subject `rem` 20201227 )


findSize'' s key  = length $ takeWhile (/=key)  $ scanl (\a b  -> doLoop s a) 1 [1..]

getLoopSizes k1 k2 = (length . takeWhile (/=k1) $ l, length . takeWhile (/=k2) $l)
    where l = iterate (doLoop 7) $ 1

stuff :: IO()
stuff = do 
    let (ls1,ls2) = getLoopSizes k1 k2 
    print $ foldl' (\a _  -> doLoop k1 a) 1 [1..ls2]
    print $ foldl' (\a _  -> doLoop k2 a) 1 [1..ls1]

runner :: IO()
runner = do
    timeIt stuff 
    -- timeIt $ putStrLn $ "findSize' k1   " ++ show ( findSize' 7 k1)
    -- timeIt $ putStrLn $ "findSize'' k1  " ++ show ( findSize'' 7 k1)
    -- timeIt $ putStrLn $ "findSize' k2   " ++ show ( findSize' 7 k2)
    -- timeIt $ putStrLn $ "findSize'' k2  " ++ show ( findSize'' 7 k2) -- why is this slower when it's faster below

    -- let ls1 = findSize' 7 k1
    -- let ls2 = findSize' 7 k2
    -- timeIt $ print $ foldl' (\a b  -> doLoop k2 a) 1 [1..(findSize'' 7 k1)]
    -- timeIt $ print $ (iterate (doLoop k2) 1  ) !! ls1

    -- timeIt $ print $ foldl' (\a b  -> doLoop k1 a) 1 [1..(findSize' 7 k2)]
    -- timeIt $ print $ (iterate (doLoop k1) 1  ) !! ls2                                   -- why is this slower
