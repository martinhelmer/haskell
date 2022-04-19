module Day13 where

splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

minsAfter :: Int -> Int -> Int
minsAfter depTime busId = busId - (depTime `mod` busId)

testInput = ["939",  "7,13,x,x,59,x,31,19"]

realRow = "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,743,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,643,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23"

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
breakup n (x:xs) = (n `rem` read x , read x): breakup skip skipxs
    where skip = n + 1 + length (takeWhile (=="x") xs)
          skipxs = dropWhile (=="x") xs

busList :: Foldable t => t Char -> [(Int, Int)]
busList = breakup 0 . splitBy ','

rowReport :: (Int, Int) -> [(Int,Int)] -> String
rowReport (i,v) xs = show v ++ ":" ++ concatMap (\(i2, v2) -> " " ++ show (i2 - i)) xs

--  && (i+9) `rem` 41 == 0  &&  (i+6) `rem` 13 == 0 && (i+16) `rem` 17 == 0
-- [(0,19),(9,41),(19,743),(6,13),(16,17),(19,29),(50,643),(19,37),(4,23)]
q = take 1  $ filter (\i -> (i  `rem` 19 == 0) && (i+9) `rem` 41 == 0  &&  (i+6) `rem` 13 == 0 && (i+16) `rem` 17 == 0) $ 
    map (\i -> i * 743 * 29 * 37 -19) $ filter (\i -> (i * 743 * 29 * 37  + 31) `rem` 643 == 0 && (i * 743 * 29 * 37  + 31) `rem` 23 == 0 ) [1..] 

r = take 1  $ filter (\i -> (i  `rem` 19 == 0) && (i+9) `rem` 41 == 0  &&  (i+6) `rem` 13 == 0 && (i+16) `rem` 17 == 0) $ 
    map (\i -> i - 19 - 31) $ filter (\i -> i `rem` 643 == 0 && i `rem` 23 == 0 ) $ map (\i -> i * 743 * 29 * 37  + 31) [1..] 

runner :: IO ()
runner = do
    print r
    -- l <- getContents
    -- let row = last $ lines l
    -- print $ doBus $ lines l
    -- -- part 2 

    -- print $  busList realRow 


-- n * 743 = j * 643 - 31 p