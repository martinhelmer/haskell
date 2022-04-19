module Day22 where 

import qualified Data.Set as Set

-- TODO: use sequence / look for hashable sets

data Game = Game [Int] [Int] (Maybe Int) (Set.Set ([Int],[Int]) )  deriving Show

startG = Game [43,36,13,11,20,25,37,38,4,18,1,8,27,23,7,22,10,5,50,40,45,26,15,32,33]
               [21,29,12,28,46,9,44,6,16,39,19,24,17,14,47,48,42,34,31,3,41,35,2,30,49]
               Nothing
               Set.empty

testG = Game [9,2,6,3,1] [5,8,4,7,10] Nothing Set.empty 


playGame :: Game -> Game
playGame g@(Game p1 p2 winner visitedStates)
    | null p1 = Game p1 p2 (Just 2) visitedStates
    | null p2 = Game p1 p2 (Just 1) visitedStates
    | Set.member (p1,p2) visitedStates = Game p1 p2 (Just 1 ) visitedStates
    | otherwise = playGame $ nextState p1 p2
    where nextState (x:xs) (y:ys)
                | getWinner == 1 = Game (xs++[x,y]) ys Nothing ((p1,p2) `Set.insert` visitedStates)
                | getWinner == 2 = Game xs (ys++[y,x]) Nothing ((p1,p2) `Set.insert` visitedStates)
                where getWinner
                       | x <= length xs && y <= length ys =  winner $ playGame (Game (take x xs) (take y ys) Nothing Set.empty )
                       | x > y = 1
                       | y > x = 2
                       where winner (Game _ _ (Just w) _ ) = w


score :: Game -> Int
score (Game p1 _ (Just 1) _ )  =  sum $ zipWith (*) (reverse p1) [1..]
score (Game _ p2 (Just 2) _ )  =  sum $ zipWith (*) (reverse p2) [1..]

main :: IO ()
main = do
    print $ score .  playGame $ startG

