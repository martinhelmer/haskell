module Day01 where
import AOCHelper

run :: IO ()
run = do
   putStrLn "Day01"
   putStr " Part1: " >> readInp "input01.txt" >>= part1 
   putStr " Part2: " >> readInp "input01.txt" >>= part2

-- part1 
part1 :: String -> IO()
part1 = print . sum . map (\x -> read x `div` 3 - 2) . lines

-- part2 
part2 :: String -> IO()
part2 = print . sum . map (fuelreq . read)  . lines

fuelreq :: Integral p => p -> p
fuelreq x | x <= 0 = 0
          | otherwise = r + fuelreq r
   where r = max (x `div` 3 - 2) 0


