
module Day12b where
import AOCHelper
import Data.List
import Data.Hashable
import GHC.Generics

data Vec3 = Vec3 Int Int Int deriving Show
newtype Vec1 = Vec1 {vv::Int} deriving (Show, Eq)

data Moon a = Moon {pos::a, vel::a} deriving (Show, Eq)

type Moons a= [Moon a]

grav1d this that | this == that = 0
                 | this < that = 1
                 | otherwise  = -1

class Moony a where
    grav :: a -> a -> a
    plus :: a -> a -> a
    energy :: a -> Int

instance Moony Vec1 where
    grav (Vec1 x) (Vec1 y) = Vec1 (grav1d x y)
    plus (Vec1 x) (Vec1 y) = Vec1 (x + y)
    energy (Vec1 x) = abs x

instance Moony Vec3 where
    grav  = v3Op grav1d
    plus  = v3Op (+)
    energy (Vec3 a b c ) = abs a + abs b + abs c

v3Op :: (Int -> Int -> Int) -> Vec3 -> Vec3 -> Vec3
v3Op o (Vec3 x y z) (Vec3 x' y' z') = Vec3 (o x x')  (o y y')  (o z z')

gravOnMoon :: Moony a => Moon a -> [Moon a] -> a
gravOnMoon m xm = foldl1 plus $ map (grav (pos m) . pos) xm

updateVelocityMoon :: Moony a => Moon a -> a -> Moon a
updateVelocityMoon m g = Moon (pos m) (plus (vel m) g)

updateVelocity ::Moony a =>  Moons a-> Moons a
updateVelocity mx = map (\m -> updateVelocityMoon m (gravOnMoon m mx)) mx

moveMoons :: Moony a => Moons a -> Moons a
moveMoons = map (\(Moon p v) -> Moon (plus p v) v)

updAndMove :: Moony a => Moons a -> Moons a
updAndMove = moveMoons . updateVelocity

totalEnergy :: Moony a => [Moon a] -> Int
totalEnergy mx = sum $  map (\m -> energy (pos m) * energy (vel m) ) mx

-- moonpos = ["<x=-7, y=-1, z=6>",
--             "<x=6, y=-9, z=-9>",
--              "<x=-12, y=2, z=-7>",
--              "<x=4, y=-17, z=-12>"]

startPos3 ::  [Vec3]
startPos3 = [Vec3 (-7) (-1) 6,Vec3 6 (-9) (-9),Vec3 (-12) 2 (-7),Vec3 4 (-17) (-12)]

v2l (Vec3 a b c ) = [a,b,c]

test1Pos = [Vec3 (-1) 0 2, Vec3 2 (-10) (-7), Vec3 4 (-8) 8, Vec3 3 5 (-1)]

startMoons3 :: [Vec3] -> Moons Vec3
startMoons3 = map (\p -> Moon p (Vec3 0 0 0 ))

pFact x n | n > x = []
          | x `rem` n == 0 = n:(pFact (x `div` n) n )
          | otherwise = pFact x (n+1 )

run :: IO ()
run = do
    print "--- Day 12: The N-Body Problem ---"
    putStr " Part1: " >> part1 >>= assertInt 11384
    putStr " Part2: " >>part2 >>= assertInt 452582583272768

part1 :: IO Int
part1  = do
    return $ totalEnergy $ iterate updAndMove (startMoons3 startPos3) !! 1000

-- -- part 2 
nit target count moons | moons == target = count 
                       | otherwise = nit target (count+1) (updAndMove moons)
part2 :: IO Int
part2 = do
    let q =  map (map (\x -> Moon (Vec1 x) (Vec1 0)))  $  transpose $  map v2l startPos3
        [xMoons, yMoons, zMoons ] = q
        a = 1 + length (takeWhile (xMoons /=)  $ drop 1 $ iterate updAndMove xMoons)
        b = 1 + length (takeWhile (yMoons /=)  $ drop 1 $ iterate updAndMove yMoons)
        c = 1 + length (takeWhile (zMoons /=)  $ drop 1 $ iterate updAndMove zMoons)

    return $ product $ foldl1 cf [pFact a 2, pFact b 2, pFact c 2]

cf a b = a ++ (b \\ a)