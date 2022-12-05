module Day04 where

run :: IO ()
run = do
   putStrLn "Day04"
   putStr "Part1: " >> return [152085..670283] >>= part1
   putStr "Part2: " >> return [152085..670283] >>= part2

-- part1 
part1 :: [Int] -> IO()
part1 l =  do
    print $ matches check l 

-- part12
part2 :: [Int] -> IO()
part2 l =  do
    print $ matches check2 l

matches :: Enum a1 => (a2 -> a1) -> [a2] -> Int
matches f = sum . map (fromEnum . f)

check :: Integral t => t -> Bool
check n = go n False 10
    where go 0 dd _ = dd
          go number hasdouble lastdig | r > lastdig = False
                                      | otherwise = go (number `div` 10) (hasdouble || r == lastdig) r
            where r = number `rem` 10

check2 :: Integral t => t -> Bool
check2 n = go n (-1) 10
    where go 0 ds _ = ds > 0
          go number ds lastdig | r > lastdig = False
                               | otherwise = go (number `div` 10) q r
            where r = number `rem` 10
                  q | r == ds = -2
                    | r == lastdig && ds == -1 = r
                    | r /= lastdig && ds == -2 = -1
                    | otherwise  = ds