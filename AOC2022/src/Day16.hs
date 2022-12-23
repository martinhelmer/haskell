{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day16 (run) where

import Text.ParserCombinators.ReadP
import AOCHelper ( assertIt, readInp,)
import Data.Char (isDigit)
import Text.RawString.QQ ( r )
import  Data.List.Split (splitOn)
import qualified Data.Map as M 
import Control.Applicative ( Alternative((<|>)) )
import MyBfs ( bfsstart, bfslen )
import Data.Maybe ( fromJust )
import Data.List (subsequences, (\\) , sort)
import System.TimeIt (timeIt)

-- Parse 
number :: ReadP Int
number = read  <$> munch (\c -> isDigit c || c =='-')

word :: ReadP String
word = munch (/= ' ')

rest :: ReadP String
rest = munch (const True)

rParse :: ReadP Valve
rParse = do
   _  <- string "Valve "
   valve <- word 
   _ <- string " has flow rate="
   rate <- number 
   _ <- string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
   Valve valve rate . splitOn ", " <$> rest 


parseRow :: String -> Valve
parseRow = fst . head . readP_to_S rParse


----   ^ parse

data Valve = Valve ValveID Flowrate [ValveID] deriving (Show)
type ValveID = String 
type Flowrate = Int 
type ValveMap = M.Map ValveID Valve
type DistMap = M.Map (ValveID, ValveID) Int 


vid :: Valve -> ValveID
vid (Valve i _ _) = i 
vrate :: Valve -> Flowrate
vrate (Valve _ rate _) = rate 
vto :: Valve -> [ValveID]
vto (Valve _ _ to) = to 

lookupRate :: ValveMap -> ValveID -> Int 
lookupRate m v = vrate $ fromJust $ M.lookup v m

data State = State { smap :: ValveMap,
                     srel ::Int,
                     srate ::Int,
                     srem :: [ValveID],
                     svid :: ValveID,
                     smin :: Int
                    }  deriving (Show)

instance Eq State where 
  s1 == s2 = finalrel s1 == finalrel s2 
instance Ord State where 
  compare s1 s2 = compare (finalrel s1) (finalrel s2)



--- current state = 
  -- released 
  -- current flow rate 
  -- remaining 
  -- current VALVE
  -- minute 

-- A is Better than B if current_loc  is SAME and released A > = released B and flow A >= flow B AND rem A >= rem B AND minute A <= minute B 

distMap :: ValveMap -> [ValveID]  -> DistMap
distMap m t = M.fromList [ ((f,t'), distFromTo m f t') | f <- td, t' <- td ]
    where distFromTo m' from to = bfslen $ bfsstart (\k ->  vto (fromJust (M.lookup k m' ))) (==to) from 
          td =  "AA" : filter (`elem` t) ( M.keys m)

finalize :: State -> State
finalize s@(State m srel srate _ svid smin) | smin == 30 = s
                                            | otherwise = State m (srel + srate * (30 - smin)) srate [] svid 30

move :: DistMap ->  State -> ValveID -> State
move d s to_id | smin s + dist + 1 >=  30 =  finalize s 
               | otherwise = State m 
                      (srel s + (dist + 1) * srate s ) 
                      (srate s + lookupRate m to_id) 
                      (filter (/=to_id) (srem s)) 
                      to_id  
                      (smin s + dist + 1)
              where dist = fromJust $ M.lookup (svid s,to_id) d 
                    m = smap s 

doit ::  DistMap -> [State] -> [State]
doit dists states =  filterout $ concatMap (\s -> if null (srem s) then [finalize s] else map (move dists s) (srem s)) states 

finalrel :: State -> Int
finalrel s = (30 - smin s)*(srate s) + (srel s) + (30 - smin s)* sum ( map (lookupRate (smap s)) (srem s))


filterout :: [State] -> [State]
filterout states = M.elems gm 
      where gm =   foldl insertifmax M.empty states
            sid s=  if null (srem s) then "X" else  concat ((svid s):(srem s))
            insertifmax m state | (finalrel <$> M.lookup (sid state) m)  > Just (finalrel state) = m 
                                | otherwise = M.insert (sid state) state m

part1 :: String  -> IO Int
part1 s  = do
  let valves = M.fromList $ map (\s' -> let row = parseRow s' in (vid row,row)) $ lines s
      toVisit =  map vid . filter (\v -> vrate v > 0) $  M.elems valves
      dists = distMap valves toVisit 
      initState = State valves 0 0 toVisit "AA" 0 
  -- print $ dists
  -- putStrLn $ unlines $ map  (concat . tail . splitOn "])]," . (show)) $  (iterate (doit dists) [initState]) !! 3
  -- print $ map length $ take (length toVisit + 2) $ (iterate (doit dists) [initState])
  let sols = iterate (doit dists) [initState] !! length toVisit
      ms = maximum sols
  print (length sols)
  -- print $ map srel sols 
  -- print $ srel ms 
  return $ srel ms

doSet :: ValveMap -> DistMap -> ([ValveID], [ValveID]) -> Int 
doSet vm dm (l1,l2) = rate l1 + rate l2 
    where rate l = srel . maximum $ (iterate (doit dm) [State vm 0 0 l "AA" 4 ]) !! (length l)

part2 ::  String -> IO Int
part2 s = do  
  let valves = M.fromList $ map (\s' -> let row = parseRow s' in (vid row,row)) $ lines s
      toVisit =  map vid . filter (\v -> vrate v > 0) $  M.elems valves
      dists = distMap valves toVisit
      sets = map (\l -> (l,toVisit \\ l)) $  filter (\s' -> length s' > 6 && length s' <= (length toVisit `div` 2)) $ subsequences $ toVisit
  return $ maximum $ map (doSet valves dists) sets 

run :: IO ()
run = do
   putStrLn "--- Day16 Valves ---"
   putStr " Part1: "
   timeIt $ readInp  "input16.txt" >>= part1 >>= assertIt 1595
   timeIt $ readInp  "input16.txt" >>= part1b >>= assertIt 1595
  --  putStr " Part2: "
  --  readInp  "input16.txt" >>= part2 >>= assertIt 2189

testR :: String
testR = "Valve AA has flow rate=0; tunnels lead to valves CV, HX, TR, MK, DQ"

runtest :: IO ()
runtest = do 
     part1b test >>= assertIt 1651
--     part2 test >>= assertIt 56000011

-- A is Better than B if current_loc  is SAME and flow A >= flow B AND visited A <= visited B AND pos A <= pos B 

test :: String
test = [r|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II|]

bfs' :: DistMap  -> Int -> [String] -> [String] -> [[String]]
bfs' dmap togo history remaining | null remaining = [history]
                                 | otherwise = concatMap (\neighbor -> bfs' dmap (rem' neighbor) (neighbor:history)  (filter (/=neighbor) neighbors) ) $ neighbors
                          where current = head history 
                                neighbors = filter (\n -> rem' n >= 0) remaining
                                rem' n = togo - (fromJust (M.lookup (current,n) dmap))

part1b :: String  -> IO Int
part1b s = do
    let valves = M.fromList $ map (\s' -> let row = parseRow s' in (vid row,row)) $ lines s
        toVisit =  map vid . filter (\v -> vrate v > 0) $  M.elems valves
        dists = distMap valves toVisit 
        l =   bfs' dists 30 ["AA"] toVisit
        sss = foldl (move dists) (State valves 0 0 toVisit "AA" 0 )
    print (length l)
    return $ head $ reverse $ sort ( map srel $ map (\x -> finalize . sss $ (tail . reverse $ x)) l)
