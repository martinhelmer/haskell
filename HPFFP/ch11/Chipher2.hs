module Chipher2 where
import Chipher 
import Data.Char as Ch

keywordToShift :: String -> [Int]
keywordToShift = map (\x -> ord x - ord 'A') 

vigenère :: String -> String -> String
vigenère key = dooah (cycle (keywordToShift key))
    where dooah :: [Int] -> String -> String
          dooah _ [] = ""
          dooah (kh:kxs) (th:txs) | th == ' '  = th:dooah (kh:kxs) txs
                                  | otherwise  = (Chipher.cipherChar kh th) : (dooah kxs txs)

