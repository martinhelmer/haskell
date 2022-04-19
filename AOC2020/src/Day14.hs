
module Day14 where 
    
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit, isDigit)
import qualified Data.Map as Map
import Data.Bits
import Data.List (foldl')

data State = State Mask (Map.Map Int Int) deriving Show
startState = State (Mask allones 0) Map.empty 

data Instruction a = NewMask a | UpdMap Int Int deriving Show

type AndMask = Int
type OrMask = Int
data Mask = Mask AndMask OrMask deriving Show

shiftML :: Mask -> Int -> Int -> Mask 
shiftML (Mask andM orM) andV orV = Mask (allones .&. (andM `shiftL` 1 + andV)) (orM `shiftL` 1 + orV )

allones = (2^36)-1


addToMask :: Mask -> Char -> Mask
addToMask m c | c == 'X' = shiftML m 1 0
              | c == '1' = shiftML m 1 1
              | c == '0' = shiftML m 0 0
              | otherwise = undefined 

parseMask :: String -> Mask
parseMask = foldl addToMask (Mask allones 0)


applyMask :: Mask -> Int -> Int 
applyMask (Mask andM orM) i = (i .&. andM ) .|. orM
showMask (Mask a b) = printAsBin 36 a ++ "\n" ++ printAsBin 36 b
testMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

printAsBin :: Int -> Int -> [Char]
printAsBin d v = drop (length l - d ) l
    where
        l  = replicate d '0' ++ showIntAtBase 2 intToDigit v ""

-- 

updateState :: State -> Instruction Mask-> State 
updateState (State mask map) (NewMask m) = State m map 
updateState (State mask map) (UpdMap k v) = State mask (Map.insert k (applyMask mask v) map)

parseLine :: String -> (String -> a) -> Instruction a
parseLine s p| take 7 s == "mask = " = NewMask $ p (drop 7 s)
             | otherwise = UpdMap k v 
             where  k = read . takeWhile isDigit . dropWhile (not . isDigit) $ s :: Int
                    v = read . tail . dropWhile (/='=') $ s ::Int


runProgram :: [String] -> State
runProgram = foldl' (\s i -> updateState s $ parseLine i parseMask) startState

getRegValues :: State -> [Int]
getRegValues (State _ m) = Map.elems m

testProgram = ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
               "mem[8] = 11",
               "mem[7] = 101",
               "mem[8] = 0"]

-- | Part 2

type InverMasks = [Mask]
data AddressMask = AddressMask OrMask InverMasks deriving Show

getOrMask :: Mask -> OrMask
getOrMask (Mask _ ormask )= ormask 
getAddressIMasks :: AddressMask -> InverMasks
getAddressIMasks (AddressMask _ i) = i
getAddressOMask :: AddressMask -> OrMask
getAddressOMask (AddressMask o _ ) = o


data State2 = State2 AddressMask (Map.Map Int Int) deriving Show

startState2 = State2 (AddressMask 0 [Mask allones 0]) Map.empty 

parseAddressMask :: String -> AddressMask
parseAddressMask s = AddressMask (getOrMask $ parseMask s) (makeInvMasks s)


replS ::String -> String -> String 
replS [] _ = []
replS l@(x:xs) [] = replicate (length l) 'X'
replS (x:xs) oo@(o:os) = if x == 'X' then o:replS xs os else 'X':replS xs  oo

makeInvMasks :: String -> [Mask] -- Apply each of these MAsks after the ormask to get addresses
makeInvMasks s = [parseMask $ replS s p | p<- perms]
    where numX = length . filter (== 'X') $ s
          perms = map (printAsBin numX) [0..(2^numX)-1]

-- | 

runProgram2 :: [String] -> State2
runProgram2 = foldl' (\s i -> updateState2 s $ parseLine i parseAddressMask) startState2

updateState2 :: State2 -> Instruction AddressMask-> State2
updateState2 (State2 mask inMap) (NewMask m) = State2 m inMap 
updateState2 (State2 mask inMap) (UpdMap k v) = State2 mask $ foldl' maskTheAddr inMap (getAddressIMasks mask)
    where maskTheAddr accMap thisMask = Map.insert (applyMask thisMask (k .|. getAddressOMask mask)) v accMap

getRegValues2 :: State2 -> [Int]
getRegValues2 (State2 _ m) = Map.elems m

testProg2 = ["mask = 000000000000000000000000000000X1001X",
             "mem[42] = 100",
             "mask = 00000000000000000000000000000000X0XX",
             "mem[26] = 1"]


main :: IO ()
main = do
    l <- getContents
    print $ sum . getRegValues . runProgram $ lines l
    print $ sum . getRegValues2 . runProgram2 $ lines l 

