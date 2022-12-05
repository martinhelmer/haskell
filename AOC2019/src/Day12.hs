module Day12 where

type Position = (Int,Int,Int)
type Velocity = (Int,Int,Int)
type Gravity = (Int,Int,Int)
data Moon = Moon {pos::Position, vel::Velocity} deriving Show
type Moons = [Moon]


grav1d this that | this == that = 0
                 | this < that = 1
                 | otherwise  = -1

tOp o (x,y,z) (x',y',z') = (o x x', o y y', o z z')
    

grav3d :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
grav3d  = tOp grav1d

gravOnMoon :: Moon -> Moons -> Gravity
gravOnMoon m xm = foldl1 (tOp (+)) $ map (grav3d (pos m) . pos) xm

updateVelocityMoon :: Moon -> Gravity -> Moon
updateVelocityMoon m g = Moon (pos m) (tOp (+) (vel m) g)

updateVelocity :: Moons -> Moons
updateVelocity mx = map (\m -> updateVelocityMoon m (gravOnMoon m mx)) mx

moveMoons :: Moons -> Moons
moveMoons = map (\(Moon p v) -> Moon (tOp (+) p v) v)

updAndMove :: Moons -> Moons
updAndMove = moveMoons . updateVelocity
energy (a,b,c) = abs a + abs b + abs c

totalEnergy :: Moons -> Int
totalEnergy = sum . map (\m -> energy (pos m) * energy (vel m) )

moonpos = ["<x=-7, y=-1, z=6>",
            "<x=6, y=-9, z=-9>",
             "<x=-12, y=2, z=-7>",
             "<x=4, y=-17, z=-12>"]

startPos :: [(Int, Int, Int)]
startPos = [(-7,-1,6),(6,-9,-9),(-12,2,-7),(4,-17,-12)]

test1Pos :: [(Int, Int, Int)]
test1Pos = [(-1,0,2),(2,-10,-7),(4,-8,8),(3,5,-1)]


startMoons = map (\p -> Moon p (0,0,0))

run :: IO ()
run = do
    part1 

part1 :: IO()
part1  = do
    print $ totalEnergy $ iterate updAndMove (startMoons startPos) !! 100000

-- part 2 
