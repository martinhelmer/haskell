import qualified Data.List as L
import Helpers


diffs l = zipWith (-) (drop 1 l) l

count n = length . filter (n ==)

oneChunks = map length . filter ((==) 1 . head) . L.group

oneCombos :: Int -> Int
oneCombos (-3) = 0
oneCombos (-2) = 0
oneCombos (-1) = 0
oneCombos 0 = 1
oneCombos 1 = 1
oneCombos 2 = 2
oneCombos n = oneCombos (-1+n) + oneCombos (-2+n) + oneCombos (-3+n)



main :: IO ()
main = do
       l <- getContents
       let sl = diffs $ 0:(L.sort $ ints l )
       print $ sl
       print $ (count 1 sl) * (1 + (count 3 sl))
       print $ (count 1 sl , (1 + count 3 sl))
       print $ (oneChunks sl)
       print $ map oneCombos (oneChunks sl)
       print $ product $ map oneCombos (oneChunks sl)


