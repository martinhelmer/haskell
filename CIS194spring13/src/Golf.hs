
f = filter
m = map
l = length

localMaxima :: (Ord c, Num c) => [c] -> [c]
localMaxima q = m (\(_,x,_) -> x) $ f (\(x,y,z) -> y>x && y>z) $ drop 2 ( zip3 q (0:q) (0:0:q))

e :: Int -> [a] -> [a]
e n b = m snd $ f (\i -> fst i == n) $ zip (cycle [1..n]) b

skips :: [a] -> [[a]]
skips b = m (`e` b) [1..l b]


histogram :: (Eq a, Num a, Enum a) => [a] -> [Char]
histogram a = h' "" $ m (\x -> l $ f (==x) a) [0..9]

h' :: (Ord b, Num b, Enum b) => [Char] -> [b] -> [Char]
h' b r | maximum r <= 0 = b ++ "==========\n0123456789\n"
            | True =  h' (m (\x -> if x <= 0 then ' ' else  '*' ) r ++"\n" ++ b)  (m pred r)