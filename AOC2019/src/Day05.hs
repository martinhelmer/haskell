module Day05 where
import           AOCHelper
import qualified Data.Vector.Unboxed as V
import Data.Maybe (isNothing)
import Control.Monad.ST
import Control.Monad
import IntCode

longTestProg = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," ++
               "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," ++
               "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

tests = [("3,9,8,9,10,9,4,9,99,-1,8", -1 , 0),
         ("3,9,8,9,10,9,4,9,99,-1,8", 8 , 1),
         ("3,9,7,9,10,9,4,9,99,-1,8", 7 , 1),
         ("3,9,7,9,10,9,4,9,99,-1,8", 8 , 0),
         ("3,3,1108,-1,8,3,4,3,99", 0 ,0),
         ("3,3,1108,-1,8,3,4,3,99",8,1),
         ("3,3,1107,-1,8,3,4,3,99", 7, 1),
         ("3,3,1107,-1,8,3,4,3,99", 8, 0),
         ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9",0,0),
         ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9",-1,1),
         ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 0,0),
         ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", -1,1),
         (longTestProg, 7 , 999),
         (longTestProg, 8, 1000),
         (longTestProg, 9, 1001)
         ]

runTests :: IO ()
runTests = do 
     forM_ tests $ \(v,i,o) -> do
        let c = runProg (parseInp v) [i]
        let out = output c
        putStrLn $ v ++ " " ++ show i ++ " " ++ show o ++ " -> " ++ show (vector c) ++ "," ++ show out ++ " " ++ show (head out == o)


run :: IO ()
run = do
   putStrLn "Day05 ..."
   putStr "Part1: " >> readInp "input05.txt" >>= part1 >>= assertInt 9775037
   putStr "Part2: " >> readInp "input05.txt" >>= part2 >>= assertInt 15586959

-- part1 
part1 :: String -> IO Int
part1 s =  do
   let vInit = parseInp s
   return $ head . output $ runProg vInit [1] 

-- part2 
part2 :: String -> IO Int
part2 s =  do
    return $ head . output $ runProg (parseInp s) [5] 


parseInp :: String -> V.Vector Int
parseInp s = V.fromList (read $ "[" ++ s ++ "]")
