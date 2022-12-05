{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Day14 where
import      Data.List.Split
import qualified Data.Map as M
import AOCHelper

test1 = unlines ["10 ORE => 10 A",
        "1 ORE => 1 B",
        "7 A, 1 B => 1 C",
        "7 A, 1 C => 1 D",
        "7 A, 1 D => 1 E",      -- E: (1,[(A,1),(D,1)])
        "7 A, 1 E => 1 FUEL"]

test2 = unlines ["9 ORE => 2 A",
                "8 ORE => 3 B",
                "7 ORE => 5 C",
                "3 A, 4 B => 1 AB",
                "5 B, 7 C => 1 BC",
                "4 C, 1 A => 1 CA",
                "2 AB, 3 BC, 4 CA => 1 FUEL"]

test3 = unlines ["157 ORE => 5 NZVS",
                "165 ORE => 6 DCFZ",
                "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
                "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
                "179 ORE => 7 PSHF",
                "177 ORE => 5 HKGWZ",
                "7 DCFZ, 7 PSHF => 2 XJWVT",
                "165 ORE => 2 GPVTF",
                "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]

prod = readInp "input14.txt"

test5 :: String
test5 = unlines ["171 ORE => 8 CNZTR",
            "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
            "114 ORE => 4 BHXH",
            "14 VRPVC => 6 BMBT",
            "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
            "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
            "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
            "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
            "5 BMBT => 4 WPTQ",
            "189 ORE => 9 KTJDG",
            "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
            "12 VRPVC, 27 CNZTR => 2 XDBXC",
            "15 KTJDG, 12 BHXH => 5 XCVML",
            "3 BHXH, 2 VRPVC => 7 MZWV",
            "121 ORE => 7 VRPVC",
            "7 XCVML => 6 RJRHP",
            "5 BHXH, 4 VRPVC => 5 LTCX"]

parseRow :: String -> (Chemical, ProdInfo)
parseRow r = (k,v)
    where
        v = (read kc::Int,components)
        components = map (s2tup . words) $ splitOn ", " s -- [("A",7),("D",1))]
        [kc, k] = words t           -- = ["1","E"]
        [s,t] =  splitOn " => " r   --  = ["7 A, 1 D","1 E"]
        s2tup [a,b] = (b, read a::Int)

s2CostMap inp= M.fromList $ map parseRow $ lines inp

type Inventory = M.Map String Int
type ProdInfo = (Int, [(String, Int)])
type CostMap = M.Map String ProdInfo
type Chemical = String

ore = "ORE"
fuel = "FUEL"

oreLevel :: Inventory -> Int
oreLevel inv = inv M.! ore

fuelLevel :: Inventory -> Int
fuelLevel inv = inv M.! fuel

multiplyInv :: Inventory -> Int -> Inventory
multiplyInv i n= M.map (n*) i


makeNBatchesOf :: CostMap -> Inventory -> Chemical -> Int -> Inventory
makeNBatchesOf cm inv chem n= M.unionWith (+) inv (M.fromList updates)
    where updates = (chem, n * fst costInfo) : map ( ((-n)*) <$>) (snd costInfo)
          costInfo = cm M.! chem

makeOneBatchOf :: CostMap -> Inventory -> Chemical -> Inventory
makeOneBatchOf cm inv chem = makeNBatchesOf cm inv chem 1

reduce :: CostMap -> M.Map String Int -> M.Map String Int
reduce cm = redprod cm (\cm k a -> k/=ore && k/=fuel && (a >= fst (cm M.! k)))

produce :: CostMap -> M.Map String Int -> M.Map String Int
produce cm = redprod cm (\cm k a -> k/=ore && a < 0)


redprod :: CostMap -> (CostMap -> [Char] -> Int -> Bool) -> M.Map String Int -> M.Map String Int
redprod cm p inv | M.null deficiencies = inv
                 | otherwise = redprod cm p $ makeNBatchesOf cm inv def amount
    where deficiencies = M.filterWithKey (p cm) inv
          def = head $ M.keys deficiencies
          amount = (-1)*(   (inv M.! def)   `div` (fst (cm M.! def))      )


produceMany :: CostMap ->  Inventory -> Inventory
produceMany cm inv | oreLevel nextProduction <= 0 = inv
                   | otherwise = produceMany cm nextProduction
                where nextProduction = produce cm (makeOneBatchOf cm inv fuel)

produceMany' :: CostMap ->  Inventory -> Inventory -> Inventory
produceMany' cm makeone inv | oreLevel inv < oneFuelCost * 2   = produceMany cm inv
                    | otherwise = produceMany' cm makeone $ reduce cm $ M.unionWith (+) inv bigChunk
                where oneFuelCost = (-1) * oreLevel makeone
                      bigChunk = multiplyInv makeone (oreLevel inv `div` oneFuelCost)

makeOneOreInv :: CostMap -> Inventory
makeOneOreInv cm = produce cm $  makeOneBatchOf cm M.empty fuel


run :: IO ()
run = do
   putStrLn "--- Day 14: Space Stoichiometry ---"
   putStr " Part1: " >> readInp "input14.txt" >>= part1 >>= assertInt 870051
   putStr " Part2: " >> readInp "input14.txt" >>= part2 >>= assertInt 1863741

part1 :: String -> IO Int
part1 s = do
    return $ (-1) * oreLevel (makeOneOreInv $ s2CostMap s)


part2 :: String -> IO Int
part2 s = do
    let oreStore = 10^12
        startInv = M.singleton ore oreStore
        cm = s2CostMap s
    return $ fuelLevel $ produceMany' cm (makeOneOreInv cm) startInv
