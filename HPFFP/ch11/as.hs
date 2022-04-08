import Data.Char as Ch
import Data.List
import Data.Maybe
import qualified Counter as Counter
import qualified Data.ByteString as C

isSubseqOf :: (Eq a) => [a]-> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ax@(x:xs) (s:ss) | x == s = isSubseqOf xs ss
                            | otherwise = isSubseqOf ax ss

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = Ch.toUpper x:xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
    where go :: [String] -> [(String, String)]
          go [] = []
          go (x:xs) = (x, capitalizeWord x) : go xs

endsWithDot x = '.' == last x

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph x = (unwords . capWordList "." . words) x
            where capWordList :: String -> [String] -> [String]
                  capWordList _ [] = []
                  capWordList prev (x:xs) = if endsWithDot prev then capitalizeWord x : (capWordList x xs) else x : (capWordList x xs)

type Digit = Char
type Presses = Int


data Button = Button Digit [Action] deriving (Show, Eq)

data Action = Cap | Value Char  deriving (Show, Eq)

buttonDigit (Button d _) = d
buttonActions (Button _ xs) = xs
buttonNumActions (Button _ xs) = length xs


data DaPhone = DaPhone [Button] deriving (Show)

capButton :: DaPhone -> Button
capButton (DaPhone x) = (head . filter hasCap) x
    where hasCap::Button -> Bool
          hasCap (Button _ xs) = Cap `elem` xs

withThisButton :: Button -> Char -> Maybe [(Digit, Presses)]
withThisButton (Button d values) c
            | c == d = Just [(c::Digit, 1+ length values::Presses)]
            | otherwise = case ix of
                            Nothing -> Nothing
                            (Just i) -> if isUpper c then Just [('*',1), (d, i + 1)] else Just [(d, i + 1)]

        where ix = elemIndex (Value (toLower c)) values



stdPhone = DaPhone [Button '1' [],
                    Button '2' [Value 'a', Value 'b', Value 'c'],
                    Button '3' [Value 'd', Value 'e', Value 'f'],
                    Button '4' [Value 'g', Value 'h', Value 'i'],
                    Button '5' [Value 'j', Value 'k', Value 'l'],
                    Button '6' [Value 'm', Value 'n', Value 'o'],
                    Button '7' [Value 'p', Value 'q', Value 'r', Value 's'],
                    Button '8' [Value 't', Value 'u', Value 'v'],
                    Button '9' [Value 'w', Value 'x', Value 'y', Value 'z'],
                    Button '0' [Value '+', Value ' ', Value '_'],
                    Button '*' [Cap],
                    Button '#' [Value '.', Value ',']
                    ]

taps :: DaPhone -> Char -> [(Digit, Presses)]
taps (DaPhone butts) c = fromJust . head . filter (/=Nothing) $ map (`withThisButton` c) butts

phButts (DaPhone butts) = butts


cellPhonesDead :: DaPhone
                    -> String
                    -> [(Digit, Presses)]
cellPhonesDead dp s = foldl' (++) [] $ map (taps dp) s


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd


mostPopularThing :: (Ord a) => [a] -> a
mostPopularThing = fst . Counter.maximum . foldl' (flip Counter.inc)  Counter.empty 
 