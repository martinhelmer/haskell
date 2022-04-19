module Day19 where 

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List

inp = "0: 4 1 5\n\
\1: 2 3 | 3 2\n\
\2: 4 4 | 5 5\n\
\3: 4 5 | 5\n\
\4: \"a\"\n\
\5: \"b\""

parseMap :: [String] -> RuleMap 
parseMap l = Map.fromList $ map go $ l
    where go row = (key,tail . tail $ value)
            where (key, value) = span (/=':') . filter (/='"') $ row
      

type RuleMap =  Map.Map String String
data Rule = SimpleRule Char 
            | Rule [Rule]
            | Rules [Rule] [Rule] deriving Show
            
parseRule ::RuleMap -> String -> Rule 
parseRule m s | length ruleString == 1 = SimpleRule $ head ruleString 
              | rs2 == "" = Rule $ map (parseRule m) $ words ruleString
              | otherwise = Rules (map (parseRule m) $ words rs1) (map (parseRule m) $ words $ tail rs2)
        where ruleString = fromMaybe "" $  Map.lookup s m
              (rs1, rs2) = span (/='|') ruleString


genPatters :: Rule -> [String]
genPatters (SimpleRule c) = [[c]] 
genPatters (Rule (x:[])) =  genPatters x
genPatters (Rule (x:xs)) =  combine (genPatters x) (genPatters $ Rule xs)
genPatters (Rules xs ys) = (genPatters $ Rule xs) ++ (genPatters $ Rule ys)


combine l1 l2 = [a++b | a<-l1, b <- l2]

combine' s l xs= nub . filter (`isPrefixOf` s) $ concatMap (\p -> [p++l | 
            l<- let ds = drop (length p) s in filter (`isPrefixOf` ds) $ fPatters ds $ Rule xs]) l

fPatters :: String -> Rule -> [String]
fPatters s (SimpleRule c) = [[c]] 
fPatters s (Rule (x:[])) =  filter (`isPrefixOf` s) $ fPatters s x
fPatters s (Rule (x:xs)) =  combine' s (filter (`isPrefixOf` s) $ fPatters s x) xs
fPatters s (Rules xs ys) = filter (`isPrefixOf` s) $ (fPatters s $ Rule xs) ++ (fPatters s $ Rule ys)

isValid r s = s `elem` fPatters s r
test1 = Map.fromList [("0", "4 1 5"),("1","2 3 | 3 2"),("2","4 4 | 5 5"),("3","4 5 | 5 4"),("4","a"),("5","b")]
test2 = Map.fromList [("0", "5 1 5"),("1","2 2"),("2","4 4"),("4","a"),("5","b")]

r2 = parseRule test2 "0"
r1 = parseRule test1 "0"

-- part 2 
ruleMods = Map.fromList  [ ("8", "42 | 42 801"), 
                             ("801", "42 | 42 802"),
                             ("802", "42 | 42 803"),
                             ("803", "42 | 42 804"),
                             ("804", "42 | 42 805"),
                             ("805", "42 | 42 806"),
                             ("806", "42"),
                             ("11","42 31 | 42 11b 31"),
                             ("11b","42 31 | 42 11c 31"),
                             ("11c","42 31 | 42 11d 31"),
                             ("11d","42 31 | 42 11e 31"),
                             ("11e","42 31 | 42 11f 31"),
                             ("11f","42 31")]

main = do
    l <- getContents
    let (ruleset, samples) = span (/="") $ lines l
    let r = parseRule (parseMap ruleset) "0"
    let p = genPatters r

    -- print $ filter (`elem` p) $ tail samples
    -- print $ nub $ genPatters $ parseRule (parseMap ruleset) "42"
    -- print $ nub $ genPatters $ parseRule (parseMap ruleset) "31"
    print "Hello"
    print $ length $ filter (isValid r) samples

    let m3 = ruleMods `Map.union` parseMap ruleset
    print $ length $ filter (isValid $ parseRule m3 "0") samples
                     