{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Day19 (run) where

import Data.List ( foldl' ) 
import AOCHelper ( assertIt, readInp)
import qualified Data.Set as S
import Data.Maybe ( fromJust, mapMaybe, isJust, isNothing )
import qualified Data.Map as M
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit ) 
-- Parse 
number :: ReadP Int
number = read  <$> munch (\c -> isDigit c || c =='-')

word :: ReadP String
word = munch (/= ' ')

rest :: ReadP String
rest = munch (const True)

-- "Blueprint 30: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 7 clay. Each geode robot costs 4 ore and 13 obsidian."
rParse :: ReadP State
rParse = do
   _  <- string "Blueprint "
   bpid <- number 
   _ <- string ": Each ore robot costs "
   oo <- number 
   _ <- string " ore. Each clay robot costs "
   co <- number 
   _ <- string " ore. Each obsidian robot costs "
   obso <- number
   _ <- string " ore and "
   obsc <- number 
   _ <- string  " clay. Each geode robot costs "
   go <- number
   _ <- string  " ore and "
   gobs <- number  
   _ <- string " obsidian."
   return $ initState (Blueprint bpid (maximum [oo,co,obso,go]) obsc (oo,0,0) (co,0,0) (obso, obsc,0) (go,0,gobs)) 

initState :: Blueprint -> State
initState bp = State 1 bp (M.fromList [(Ore,1),(Clay,0),(Obsid,0),(Geode,0)]) (M.fromList $ zip materials [0,0,0,0])

parseRow :: String -> State
parseRow = fst . head . readP_to_S rParse

-- 
mapInc m k v = M.adjust (v + ) k m

--


materials = [Ore, Clay, Obsid, Geode] 

type Buildcost = (Int, Int, Int)
type Robots = M.Map OreType Int
type Stash = M.Map OreType Int
type Timer = Int 
data OreType = Ore | Clay | Obsid | Geode deriving (Eq, Ord, Show)
type Robot = OreType

data Blueprint = Blueprint { pb_id :: Int, 
                             max_ore :: Int, 
                             max_clay :: Int, 
                             cost_ore :: Buildcost,
                             cost_clay :: Buildcost,
                             cost_obsid :: Buildcost,
                             cost_geode ::Buildcost } deriving (Show, Eq, Ord)

data State = State Timer Blueprint Robots Stash deriving (Eq, Ord)

-- "Blueprint 30: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 7 clay. Each geode robot costs 4 ore and 13 obsidian."

ts = map parseRow $ lines test 
test = unlines ["Blueprint 1: Each ore robot costs 4 ore. " ++
                "Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. " ++
                "Each geode robot costs 2 ore and 7 obsidian.",
                "Blueprint 2: "++
                "Each ore robot costs 2 ore. "++
                "Each clay robot costs 3 ore. "++
                "Each obsidian robot costs 3 ore and 8 clay. "++
                "Each geode robot costs 3 ore and 12 obsidian."]

bp1 = Blueprint 1 4 14 (4,0,0) (2,0,0) (3,14,0) (2,0,7)
bp2 = Blueprint 2 3 8 (2,0,0) (3,0,0) (3,8,0) (3,0,12)

frommap stash material =  fromJust (M.lookup material stash) 

robotcost :: Blueprint -> OreType -> Buildcost
robotcost bp robot = case robot of 
                            Ore -> cost_ore bp 
                            Clay -> cost_clay bp
                            Obsid -> cost_obsid bp
                            Geode -> cost_geode bp 

instance Show State where 
  show (State t bp r st) = 
        "\n\n== Minute "++show t ++ "== of bp " ++ show (pb_id bp) ++
        "\n PROD " ++ show (M.assocs r) ++
        "\n STASH" ++ show (M.assocs st) ++
        "\n BP   " ++ show bp 

orecost :: Buildcost -> Int 
orecost (o, _, _) = o       
claycost (_, c, _) = c          
obsidcost (_,_,b) = b 


getScore (State t bp r st) = frommap st Geode

getbpid (State t bp r st) = pb_id bp 

getquality s = getScore s * getbpid s  

getGeoProd (State t bp r st) = frommap r Geode

produce (State t bp r st) st2 = State (t+1) bp r (M.unionWith (+) st st2)

production (State t bp r st) = r 

addMaterial  (State t bp r st) mat n = State t bp r (mapInc st mat n)


state_time (State t _ _ _ ) = t 

smax s1 s2 = if getScore s1 > getScore s2 then s1 else s2  
--  
buildrobot :: Buildcost -> Stash -> Maybe Stash 
buildrobot bc st = if any (<0) l then Nothing else 
                    Just (M.fromList ( zip [Ore,Clay,Obsid, Geode ] l ))
      where l = [frommap st Ore - orecost bc, frommap st Clay - claycost bc, frommap st Obsid - obsidcost bc, frommap st Geode ]

makerobot :: State ->  OreType -> Maybe State
makerobot (State t bp rl stash) r =State t bp (mapInc rl r 1) <$> buildrobot (robotcost bp r) stash

neightbors :: Int -> State -> [State]
neightbors n s@(State t bp rl stash) | t > n = []
                                   | t == n = [produce s (production s)]
                                   | isJust makegeode = [produce (fromJust makegeode) (production s)]
                                   | t == (n-1) = [produce s (production s)]
                                   | isJust makeobs = [produce (fromJust makeobs) (production s)]
                                   | frommap stash Ore - 2 * (max_ore bp) + frommap rl Ore >= 0 =  map (\n' -> produce n' (production s)) $ mapMaybe (makerobot s) mats
                                   | otherwise =  map (\n' -> produce n' (production s)) $  s : mapMaybe (makerobot s) mats
          where makegeode = makerobot s Geode 
                makeobs = makerobot s Obsid
                mats = [Ore, Clay] 

findOne ::  Int -> State -> [State]
findOne n s@(State t bp rl stash) | t > n  = [s]  
                                    | otherwise = concatMap (findOne n) (neightbors n s )

part1 ::  String -> IO Int
part1 s = do
  let states = map parseRow $ lines s  
      beststates = map (\st -> dfs' 24 st st) states 
  print $ map getScore beststates
  return $ sum $ map getquality beststates


part2 ::  String -> IO Int
part2 s = do 
  let states = take 3 $ map parseRow $ lines s  
  let beststates = map (\st -> dfs' 32 st st) states  
  return $ product $ map getScore beststates

run :: IO ()
run = do
   putStrLn "--- Day19 Robot mining ---"
   putStr " Part1b: "
   readInp  "input19.txt" >>= part1b >>= assertIt 994 
  --  putStr " Part1:  "
  --  readInp  "input19.txt" >>= part1 >>= assertIt 994 
   putStr " Part2b: "
   readInp  "input19.txt" >>= part2b >>= assertIt 15960


runtest :: IO ()
runtest = do 
     part1 test >>= assertIt 33
     part2 test >>= assertIt 3348

geom = scanl (+) 0 [1..]
upperbound n s = getScore s + rem' *  getGeoProd s + geom !! rem' 
      where rem' = 1 + n - state_time s

dfs' :: Int -> State ->  State -> State 
dfs' n best current | state_time current > n = smax best current
                    | upperbound n current < getScore best = best 
                    | otherwise = foldl'  (\b nei -> let d = dfs' n b nei in smax d b  ) best (neightbors n current)


part1b ::  String -> IO Int
part1b s = do 
  return $ sum . map (getquality .  (\st -> jump 24 st st))    $ map parseRow $ lines s  

part2b ::  String -> IO Int
part2b s = do 
  return $ product . map (getScore . (\st -> jump 32 st st)) . take 3 . map parseRow $ lines s  

jump :: Int -> State -> State -> State
jump n best current@(State t bp prod stash) 
                 | t > n = smax best current
                 | t < n && upperbound n current < getScore best = best 
                 | otherwise = foldl'  (\b nei -> let d = jump n b nei in smax d b  ) best $ neighbors n current 

neighbors n current@(State t bp prod stash) = if null producing then [toEnd] else producing
                where 
                producing =  mapMaybe jumpforward [Geode, Obsid, Ore, Clay]
                
                jumpforward robot 
                             | robot == Ore && frommap prod Ore >= max_ore bp = Nothing 
                             | robot == Clay && frommap prod Clay >= max_clay bp = Nothing 
                             | isNothing steps = Nothing 
                             | t + steps' > n  = Nothing 
                             | otherwise = Just $ State (t+steps') 
                                                        bp  
                                                        (mapInc prod robot 1 ) 
                                                        (M.unionWith (-) 
                                                               (M.unionWith (+) stash (M.map (steps'*) prod)) 
                                                               (M.fromList  ( zip [Ore,Clay,Obsid ] [orecost rcost, claycost rcost, obsidcost rcost ] )))
                             where rcost = robotcost bp robot
                                   steps =  (1+) <$> (rsub rcost (instash' stash) `rdiv` (frommap prod Ore, frommap prod Clay, frommap prod Obsid))
                                   steps' = fromJust steps 
                toEnd = State (n+1) bp prod (M.unionWith (+) stash (M.map ((1 + n - t)*) prod))

instash' :: Stash  -> Buildcost
instash' stash  = (frommap stash Ore, frommap stash Clay, frommap stash Obsid)

rsub (a,b,c) (d,e,f) = (a-d,b-e,c-f)

rdiv :: Buildcost -> Buildcost -> Maybe Int 
rdiv (a1,a2,a3) (b1,b2,b3) =foldl (\a e-> if a == Nothing || e == Nothing then Nothing else max a e) (Just 0) [(a1) `ndiv` b1, (a2) `ndiv` b2, (a3) `ndiv` b3]
  where ndiv a b | a <=0 = Just 0 
                 | a > 0 && b == 0 = Nothing
                 | otherwise =  Just (1 + ((a-1) `div` b)) 