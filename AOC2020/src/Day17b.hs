
module Day17b where 
import qualified  Data.Map as Map
import Data.Maybe
import Data.List

type Index = (Int,Int,Int,Int)
type State = Map.Map Index Bool


neighSum :: State -> Index -> Int
neighSum s = sum . map (\ix -> maybe 0 fromEnum (Map.lookup ix s)) . neighborKeys

neighborKeys :: Index -> [Index]
neighborKeys (x,y,z,w) = [(a,b,c,d) | a <- r x, b <- r y, c <- r z , d <- r w, (a,b,c,d) /= (x,y,z,w)]
    where r i = [(i-1)..(i+1)]

newCellValue :: State -> Index -> Bool
newCellValue s ix = calc (fromMaybe False (Map.lookup ix s)) $ neighSum s ix
    where calc v b = case (v,b) of
                    (_,3) -> True
                    (True, 2) -> True
                    (_,_) -> False

emptyMap s = Map.fromList (zip (empties s)(repeat False))

empties :: State -> [Index]
empties m = (nub . concatMap neighborKeys $ filter (m Map.!) $ Map.keys m) \\ Map.keys m

nextState :: State -> State
nextState s = Map.fromList $ foldl' (\ns ix -> (ix,newCellValue s ix):ns) [] (Map.keys start)
    where start = emptyMap s `Map.union` s

test = [".#.",
        "..#",
        "###"]

parseInput :: [String] -> [(Index,Bool)]
parseInput xs = concatMap f $ zip [0..] (map (zip [0..]) xs)
   where     f :: (Int, [(Int, Char)]) -> [(Index, Bool)]
             f (a,b) = map (\i -> ((a,fst i,0,0),snd i == '#')) b

main = do
    l <- getContents
    let s = Map.fromList $ parseInput $ lines l
    print $ length $ filter id $ Map.elems $ last  $ take 7 $ iterate nextState s
