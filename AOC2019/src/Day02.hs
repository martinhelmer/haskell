module Day02 where
import           AOCHelper
import qualified Data.Vector as V
import Data.Maybe (isNothing)




run :: IO ()
run = do
   putStrLn "Day02"
   putStr " Part1: " >> readInp "input02.txt" >>= part1
   putStr " Part2: " >> readInp "input02.txt" >>= part2

-- part1 
part1 :: String -> IO()
part1 s =  do
   let vInit = parseInp s 
   let  v = progResult (0, vInit V.// [(1,12),(2,2)])
   print $ v V.! 0


parseInp :: String -> V.Vector Int
parseInp s = V.fromList (read $ "[" ++ s ++ "]")

doInstr (p,v) = case v V.! p of
                  99 -> (-1, v)
                  1 -> (p+4, v V.// [(v V.! (p+3), uncurry (+) inputs)])
                  2 -> (p+4, v V.// [(v V.! (p+3), uncurry (*) inputs)])
                  _ -> undefined
   where
      inputs = (v V.! (v V.! (p+1)), v V.! (v V.! (p+2))) 

progResult vInit = snd . head $ dropWhile ((<=) 0 . fst ) $ iterate doInstr vInit

-- part2 
part2 :: String -> IO()
part2 s =  do
      let vInit = parseInp s 
      let v = head . dropWhile isNothing $ map (f vInit) $ (,) <$> [0..99] <*> [0..99]
      print $ (\(Just (a, b)) -> a * 100 + b) v

f vect t@(n,v) = if progResult (0, vect V.// [(1,n),(2,v)]) V.! 0 == 19690720 then Just t else Nothing