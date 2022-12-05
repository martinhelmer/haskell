module Day15 where
import AOCHelper
import IntCode
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as UV
import Data.Maybe

move (x,y) d = case d of
                1 -> (x,y-1)
                2 -> (x,y+1)
                3 -> (x-1,y)
                4 -> (x+1,y)
                _ -> undefined

type Pos = (Int,Int)
type Visited = S.Set Pos
data State = State PComputer Visited Pos
startState c = State c S.empty (0,0)

recit :: State -> Maybe [PComputer]
recit s@(State _ visited current) = shortest paths
    where paths = map  (doMove s) $ filter (\i -> S.notMember (move current i) visited) [1..4]
          shortest [] = Nothing
          shortest xs = maximumOn  (fmap (\l -> (-1) * length l)) xs


doMove :: State -> Int -> Maybe [PComputer]
doMove  (State robot visited current) i =
        case head $ output movedRobot of
            0 -> Nothing
            2 -> Just [movedRobot]
            1 -> (\x -> Just (movedRobot:x)) =<< recit (State movedRobot (S.insert current visited) (move current i))
            _ -> undefined
        where
          movedRobot = runComp $addInp robot i

run :: IO ()
run = do
   putStrLn "--- Day 15: Oxygen System ---"
   putStr " Part1: "
   (p1, robotAtOxygen) <- readInp "input15.txt" >>= part1
   assertInt 300 p1
   putStr " Part2: " 
   part2 robotAtOxygen >>= assertInt 312

part1 :: String -> IO (Int, PComputer)
part1 s = do
    let comp = initComp ( parseString s :: UV.Vector Int  ) []
        ss = startState comp
        findOxygen = fromJust $ recit ss
    return (length findOxygen, clearOut . last $ findOxygen )

part2 :: PComputer -> IO Int
part2 robotAtOxygen = do
    let c =  expandOxygen (S.singleton (0,0)) [((0,0),robotAtOxygen)]
    return $ length $ output c


-- part 2 
expandOxygen :: Visited -> [(Pos, PComputer)] -> PComputer
expandOxygen v xs = case newspots of
                [] -> snd . head $ xs
                _ ->  expandOxygen (S.union v (S.fromList (map fst newspots))) newspots
    where newspots = concatMap (do1spot v ) xs

do1spot ::S.Set Pos -> (Pos, PComputer) -> [(Pos, PComputer)]
do1spot visited (currentpos, currentcomp) = expansions
    where expansions = mapMaybe expand (filter (\i -> S.notMember (move currentpos i) visited) [1..4])
          expand i = case head $ output movedRobot of
                            0 -> Nothing
                            _ -> Just (move currentpos i , movedRobot)
                    where movedRobot = runComp $addInp currentcomp i