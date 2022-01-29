module Day08
    ( runner
    , myRead
    ) where
      
import qualified Data.Set as S
import Data.List ()
import qualified Data.Map as Map
import Data.Maybe ()

myRead :: String -> Int
myRead [] = 0
myRead (x:xs) = if x == '+' then read xs else read (x:xs)

getCmdArg :: String -> (String, String)
getCmdArg s = (take 3 s, drop 4 s)


accUntilRepeated :: [String] -> Int -> Int -> S.Set Int -> Int
accUntilRepeated program position acc visited  
           | position `S.member` visited = -acc
           | position >= length program = acc
           | command == "nop" = accUntilRepeated program (position + 1) acc updatedSet 
           | command == "acc" = accUntilRepeated program (position + 1) (acc + myRead arg) updatedSet 
           | command == "jmp" = accUntilRepeated program (position + myRead arg) acc updatedSet 
           | otherwise        = error "Unexpected condition in accUntilRepeated"
           where
               (command,arg) = getCmdArg $ program !! position
               updatedSet = S.insert position visited

-- part 2 --

variations :: [String] -> Int -> [[String]] -> [[String]]
variations program pos l 
          | pos >= length program = l
          | command `elem` ["nop","jmp"] = variations program (pos+1) $ replaceNth pos (flipNopJmp command arg) program:l
          | otherwise  = variations program (pos+1) l
          where 
             (command,arg) = getCmdArg $ program !! pos

             flipNopJmp :: String -> String -> String
             flipNopJmp "nop" arg = "jmp " ++ arg
             flipNopJmp "jmp" arg = "nop " ++ arg
             flipNopJmp _ _ = error "Unexpected condition"


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


runner :: IO ()
runner = do
       l <- getContents
       print ( accUntilRepeated (lines l) 0 0 S.empty )   -- part 1
       print ( head $ filter (>0) $ map (\l -> accUntilRepeated l 0 0 S.empty ) (variations (lines l) 0 []) )
