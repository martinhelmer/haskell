-- change(5, [1, 2, 5])
-- =============================
-- 1 1 1 1 1
-- 1 1 1 2
-- 1 2 2
-- 5

change _ [] = []
change amount coins@(x:xs) |  amount == x = [amount] : change amount xs
                           |  otherwise = map (x:) (change (amount-x ) $ filter (`elem` [x..amount-x]) coins) ++ change amount xs




-- change 5, [1, 2, 5] =  addToLists 1 change 4 [1,2] ++ addToLists 2 change 3 [2] ++ addToLists 5 [[]]

-- change 3 [2] = addtolists 2 change 1 []

-- change :: Int -> [Int] -> [[Int]]
-- change _ [] = []
-- change amount coins@(x:xs) =  loopCoins amount coins

-- loopCoins _ [] = []
-- loopCoins amount coins@(x:xs) |  amount == x = [amount] : loopCoins amount xs
--                               |  otherwise = addToLists x (change (amount-x ) [coin | coin <- coins, coin >= x && coin <= amount-x]) ++ loopCoins amount xs


-- addToLists :: Int -> [[Int]] -> [[Int]]
-- addToLists _ [] = []
-- addToLists i (l:ls) = (i:l): addToLists i ls 

