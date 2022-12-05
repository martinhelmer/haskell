module Day02b where
import           AOCHelper
import qualified Data.Vector.Unboxed as V
import Data.Maybe (isNothing)
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad
import Data.Functor

import IntCode

run :: IO ()
run = do
   putStrLn "Day02b ..."
   putStr " Part1: " 
   readInp "input02.txt" >>= part1 >>= assertInt 5534943 
   putStr " Part2: " 
   readInp "input02.txt" >>= part2 >>= assertInt 7603

-- part1 
part1 :: String -> IO Int 
part1 s =  do
   let ic = initComp (parseInp s) []
   let c =  runST (runProgram ic [(1,12), (2,2)] )
   return  $ vector c V.! 0

-- part2 
part2 :: String -> IO Int
part2 s =  do
      let vInit = parseInp s
      let v = head . dropWhile isNothing $ map (f vInit) $ (,) <$> [0..99] <*> [0..99]
      return  $ (\(Just (a, b)) -> a * 100 + b) v

f :: V.Vector Int -> (Int, Int) -> Maybe (Int, Int)
f sVect t@(n,v) = if vector rc V.! 0 == 19690720 then Just t else Nothing
      where ic = initComp sVect []
            rc = runST $ runProgram ic [(1,n),(2,v)]


parseInp :: String -> V.Vector Int
parseInp s = V.fromList (read $ "[" ++ s ++ "]")
