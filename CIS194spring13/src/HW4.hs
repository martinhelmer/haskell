import Data.STRef (newSTRef)
import Data.Bits (Bits(xor))
-- exercise 1

exercise1 = do
      print $ fun1 [6, 18, 4] == fun1' [6, 18, 4]
      print $ fun2 10 == fun2' 10

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

-- mine
fun1' :: (Foldable t, Integral a) => t a -> a
fun1' = foldl (\acc i -> if even i then acc*(i-2) else acc) 1

-- better
fun1'' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integral a => a -> a
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div`2 else 3 * n + 1)

-- exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
      deriving (Show, Eq)


getDepth :: Tree a -> Integer 
getDepth Leaf = -1
getDepth (Node d _ _ _) = d


insert :: Tree a -> a -> Tree a
insert Leaf a = Node 0 Leaf a Leaf
insert (Node d lTree v rTree) a | getDepth lTree < getDepth rTree = Node d (insert lTree a) v rTree
                                | otherwise = Node (1 + getDepth newR) lTree v newR 
                                      where newR = insert rTree a


foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf





main = do
    exercise1