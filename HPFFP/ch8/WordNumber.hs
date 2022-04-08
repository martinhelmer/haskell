module WordNumber where
import Data.List (intersperse, intercalate)

digitToWord :: Int -> String 
digitToWord n = case n of 
            1 -> "one"
            2 -> "two"
            3 -> "three"
            4 -> "four"
            5 -> "five"
            6 -> "six"
            7 -> "seven"
            8 -> "eight"
            9 -> "nine"
            0 -> "zero"
            _  -> undefined 
                


digits :: Int -> [Int] 
digits 0 = [0]
digits n = go n
    where
        go :: Int -> [Int]
        go 0 = []
        go n = go ( n `div` 10 ) ++ [rem n 10]  

wordNumber :: Int -> String 
wordNumber = intercalate "-" . map digitToWord . digits 

main = do
    print $ wordNumber 873201