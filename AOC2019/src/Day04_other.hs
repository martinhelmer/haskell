module Day04_other where

import Data.List (groupBy, sort)


-- Part I

adjacentDigitsAreSame :: Int -> Bool
adjacentDigitsAreSame =
    any (>=2) . map length . groupBy (==) . show


digitsIncreaseOrAreSame :: Int -> Bool
digitsIncreaseOrAreSame xs =
    show xs == (sort $ show xs)


meetsCriteria :: [Int -> Bool] -> Int -> Bool
meetsCriteria criterias x =
    and $ map ($ x) criterias


numbersInRangeMeetingCriteria :: [Int -> Bool] -> [Int] -> Int
numbersInRangeMeetingCriteria criterias =
    sum . map (fromEnum . meetsCriteria criterias)


p1 :: [Int] -> Int
p1 = numbersInRangeMeetingCriteria
    [ adjacentDigitsAreSame
    , digitsIncreaseOrAreSame
    ]


-- Part II

matchingAdjecentNumberAreNotPartOfLargerGroup :: Int -> Bool
matchingAdjecentNumberAreNotPartOfLargerGroup =
    any (==2) . map length . groupBy (==) . show


p2 :: [Int] -> Int
p2 = numbersInRangeMeetingCriteria
    [ adjacentDigitsAreSame             -- unnecessary /govert
    , digitsIncreaseOrAreSame
    , matchingAdjecentNumberAreNotPartOfLargerGroup
    ]


run :: IO ()
run = do
    putStrLn "Day04 https://github.com/sherubthakur/aoc19/blob/master/src/D04SecureContainer.hs"
    putStr " Part1: "
    print $ p1 [152085..670283]
    putStr  " Part2: "
    print $ p2 [152085..670283]
