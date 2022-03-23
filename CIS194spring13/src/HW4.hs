import Data.STRef (newSTRef)
import Data.Bits (Bits(xor))
import qualified Data.Set as Set
import Data.List

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

exercise2 = do
      print $ foldTree "ABCDEFGHIJ"
      print $  foldTree "ABCDEFGHIJ" == 
                   Node 3 
                        (Node 2 
                              (Node 0 Leaf 'H' Leaf)
                              'C' 
                              (Node 1 Leaf 'E' (Node 0 Leaf 'I' Leaf))) 
                        'A' 
                        (Node 2 
                                (Node 1 
                                    Leaf 'F' (Node 0 Leaf 'J' Leaf)) 
                                'B' 
                                (Node 1 
                                    Leaf 'D' (Node 0 Leaf 'G' Leaf)))

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
      deriving (Show, Eq)


getDepth :: Tree a -> Integer 
getDepth Leaf = -1
getDepth (Node d _ _ _) = d


insertL :: Tree a -> a -> Tree a
insertL Leaf a = Node 0 Leaf a Leaf
insertL (Node d lTree v rTree) a | getDepth lTree < getDepth rTree = Node d (insertL lTree a) v rTree
                                 | otherwise = Node (1 + getDepth newR) lTree v newR 
                                      where newR = insertL rTree a


foldTree :: [a] -> Tree a
foldTree = foldl insertL Leaf 

-- exercise 3
exercise3 = do 
      print $ Main.xor [False, True, False] == True
      print $ Main.xor [False, True, False, False, True] == False
      print $ foldl (-) 10 [1,2,3] == myFoldl (-) 10 [1,2,3]

xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []


myFoldl :: (a -> b -> a) -> a -> [b]  -> a 
myFoldl f base xs= foldr (flip f) base (reverse xs)


-- exercise 4
-- https://searchcode.com/codesearch/view/79836348/
exercise4 = do
      print $ sieveSundaram 100 == [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
                                    43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 
                                    97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 
                                    149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199]


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ Set.toList . sieve1 1 $ Set.fromList [1..n]
      where 
            sieve1 :: Integer -> Set.Set Integer  -> Set.Set Integer 
            sieve1 i s | ff i i > Set.findMax s = s
                       | otherwise = sieve1 (i+1) $ sieve2 i i s

            sieve2 :: Integer -> Integer -> Set.Set Integer -> Set.Set Integer 
            sieve2 i j s | ff i j > Set.findMax s = s
                         | otherwise = sieve2 i (j+1) $ Set.delete (ff i j) s
 
            ff i j = i + j + 2 * i * j

main = do
    exercise1
    exercise2
    exercise3
    exercise4


       


