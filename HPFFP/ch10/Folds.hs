
-- Warm-up and review

-- foldl (flip (*)) 1 [1..3] =
-- (((1 flip * 1) flip * 2) flip * 3) =
-- 3 * (2 * (1 * 1)) = 6

pelem :: a -> [[a]] -> [[a]]
pelem a [] = []
pelem a (x:xs) = (a:x) : (pelem a xs)


-- perm [[1,2],[3,4]] = 1 `pelem` perm [[3,4]] ++ perm [[2], [3,4]]   
perm :: [[a]] -> [[a]]
perm [] = []
perm ([]:_) = []
perm (x:[]) = map(:[]) x
perm (x:xs) = head x `pelem` (perm xs) ++ perm (tail x : xs)

-- nvn ["stol","hus"] ["springer","blir"]
-- [["stol","springer","stol"],["stol","springer","hus"],["stol","blir","stol"],["stol","blir","hus"],["hus","springer","stol"],["hus","springer","hus"],["hus","blir","stol"],["hus","blir","hus"]]
nvn n v = perm [n, v, n]


seekritFunc x =  (/) (fromIntegral . sum . map length . words $ x)  (fromIntegral . length . words $ x)


myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 


myMap :: (a -> b) -> [a] -> [b] 
myMap f = foldr (\x y -> (f x ):y) [] 
