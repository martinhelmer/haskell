module Day23c where

import           System.TimeIt
import           Data.Int
import           Data.Array.Unboxed
import           Data.Array.ST
import           Control.Monad
import           Data.Vector.Generic (Vector(basicLength))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M


parseInp :: (Show a , Read b) => a -> [b]
parseInp i = read ("[" ++ go (show i) ++ "]")
    where go (x:[]) = x:[]
          go (x:xs) = x:',':go xs

type TheV = V.Vector Int

getStartVector :: Show a => a -> Int -> TheV
getStartVector i s = V.fromList [1..s+1] V.// ((s,head pi):(0,head pi):zip pi (tail pi ++ [1+length pi]))
                     where pi = parseInp i

-- pick :: TheV -> [Int]
-- pick v = take 3 $ iterate (v V.!) (v V.! current v)
pickM :: PrimMonad m => M.MVector (PrimState m) Int -> Int -> m [Int]
pickM v c= do
      pick1 <- M.read v c
      pick2 <- M.read v pick1
      pick3 <- M.read v pick2
      return [pick1, pick2, pick3]

-- current :: TheV -> Int
-- current v = fromIntegral $ v V.! 0
currentM :: (PrimMonad m, M.Unbox a) => M.MVector (PrimState m) a -> m a
currentM v = M.read v 0

-- nextCurr ::TheV -> Int
-- nextCurr v = v V.! last (pick v)
-- nextCurrM :: PrimMonad m => M.MVector (PrimState m) Int -> m Int
-- nextCurrM v = pickM v >>= return . last >>= M.read v


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

-- nextState3 :: TheV -> TheV
-- nextState3 v = v V.// [(0,fromIntegral nc),(curr,fromIntegral nc), (fromIntegral d,fromIntegral $ head pck), (last pck, v V.! d)]
--        where m = basicLength v - 1
--              nc =  nextCurr v
--              pck = pick v
--              curr = fromIntegral $ current v
--              d = dest m pck curr

doMovesM :: (PrimMonad m)
         =>  Int -- ^ count 
         -> M.MVector (PrimState m) Int
         -> m ()
doMovesM n v = do
      forM_ [1..n] $ \i -> do
            let m = M.length v - 1
            curr <- currentM v
            pck <- pickM v curr
            nc <-  M.read v (last pck)
            let d = dest m pck curr
            M.write v 0 nc
            M.write v curr nc
            M.read v d >>= M.write v (last pck)
            M.write v d (head pck)


rollAround :: TheV -> Int -> TheV
rollAround v n = V.modify (doMovesM n) v


-- 523764819
nIter = 10000000
nSize = 1000000

runner :: IO ()
runner = do
      putStrLn  "Starting"
      let startVector = getStartVector 523764819 nSize
      print $ take 5 $ fromCicrular startVector
      putStrLn "Vector initialized"
      let v = rollAround startVector nIter
      -- take 9 $ fromCicrular = [2,8,9,1,5,4,6,7,10]
      print $ take 5 $ fromCicrular v
      let a = v V.! 1
      let b = v V.! (v V.! 1)
      print $ a * b




