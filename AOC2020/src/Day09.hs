module Main where
      
import qualified Data.Set as S
import Data.List ()
import qualified Data.Map as Map
import Data.Maybe ()

type NTerms = Int

sumUp :: NTerms -> Int -> [Int] -> Bool 
sumUp 0 _ _ = False 
sumUp n _ [] = False
sumUp 1 v xs = v `elem` xs
sumUp n v (x:xs) = (sumUp n v xs) ||  sumUp (n-1) (v-x) xs 

type Preamble = [Int]
checkit :: Preamble -> [Int] -> Maybe (Int, [Int])
checkit _ [] = Nothing 
checkit p (x:xs) = if sumUp 2 x p then checkit ((drop 1 p) ++ [x]) xs else Just (x, p)

-- ex 1 sol
theNum = 22406676  

type Target = Int 
type List = [Int] 


doSum :: Int -> [Int] ->  [Maybe Int]
doSum _ [] = [Nothing]
doSum t (x:xs) | t < 0 = [Nothing]
                 | t == 0 = []
                 | otherwise =  Just x : doSum (t-x) xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (flip go) (Just [])
    where go :: Maybe [a] -> Maybe a -> Maybe [a]
          go Nothing _ = Nothing
          go _ Nothing = Nothing
          go (Just xs) (Just y) = Just (y:xs)

getSum t = flipMaybe . doSum t

ex2 :: Int -> [Int] -> Maybe [Int] 
ex2 t x = head $ filter (/=Nothing) $ map (getSum t) $ scanl (\(a:ax) x -> ax) x x 

main :: IO ()
main = do
       l <- getContents
       let ints =  (map (\x -> read x ::Int ) . lines) l
       print $ checkit (take 25 ints) (drop 25 ints)
       print $  (\(Just l) -> (maximum l) + (minimum l)) $ ex2 theNum ints
