module StringProc where

import qualified Data.Char as Char
import System.FilePath (isValid)

notThe :: String -> Maybe String 
notThe s | "THE" == map Char.toUpper s = Nothing 
         | otherwise = Just s
     
replaceThe :: String -> String
replaceThe = unwords . map (aThe . notThe) . words
    where aThe :: Maybe String -> String 
          aThe Nothing = "a"
          aThe (Just x)  = x


type WordCount = Int 
type PrevWasThe = Bool 

data TheCount = TheCount WordCount PrevWasThe deriving Show

getCount (TheCount c _) = c 

countTheBeforeVowel :: String -> WordCount 
countTheBeforeVowel = getCount . foldl countIt (TheCount 0 False) . map notThe . words
    where countIt :: TheCount -> Maybe String -> TheCount 
          countIt (TheCount count _) Nothing = TheCount count True
          countIt (TheCount count True) (Just s) = TheCount (count+(if startsWithVowel s then 1 else 0)) False 
          countIt (TheCount count False) (Just s) = TheCount count False
           
          startsWithVowel :: String -> Bool 
          startsWithVowel [] =False
          startsWithVowel (x:_) = isVowel x

vowels = "aeiou" 
isVowel =  flip elem vowels . Char.toLower

countVowels :: String -> Int
countVowels = length . filter isVowel

newtype Word' =
    Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word' 
mkWord s | vows > cons = Nothing 
         | otherwise = Just (Word' s)
         where vows = countVowels s
               cons = length s - vows 

