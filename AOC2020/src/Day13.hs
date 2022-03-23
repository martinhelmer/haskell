splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

minsAfter :: Int -> Int -> Int
minsAfter depTime busId = busId - (depTime `mod` busId)

testInput = ["939",  "7,13,x,x,59,x,31,19"]

nextBus :: Int -> [Int] -> (Int, Int)
nextBus depTime buses = minimum $ zip (map (minsAfter depTime) buses) buses

parseBuses :: String -> [Int]
parseBuses = map read . filter (/="x") . splitBy ','

doBus :: [String] -> Int
doBus l = uncurry (*) go
    where go = nextBus (read $ head l) (parseBuses $ last l)


-- ex 2 
breakup :: Int -> [String] -> [(Int, Int)]
breakup _ [] = []
breakup n (x:xs) = (n `rem` (read x) , read x): breakup skip skipxs
    where skip = n + 1 + (length $ takeWhile (=="x") xs)
          skipxs = dropWhile (=="x") xs



main :: IO ()
main = do
    l <- getContents
    let row = last $ lines l
    print $ doBus $ lines l
    print $ breakup 0 $ splitBy ',' row
