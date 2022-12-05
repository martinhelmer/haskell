module Day11 where
import           AOCHelper
import           IntCode
import           Data.Maybe 
import           Data.List.Split
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as M

type Color = Int
type Position = (Int,Int)
type Direction = (Int, Int)

data Robot = Robot {robotPos::Position,
                     robotDir::Direction,
                     robotC::PComputer}  deriving Show

type Floor = M.Map (Int,Int) Int

initRobot :: PComputer -> Robot
initRobot = Robot (0,0) (0,1)


turn :: Robot -> Int -> Robot
turn (Robot p (x,y) c) 0 = Robot p (-y , x ) c-- left
turn (Robot p (x,y) c) 1 = Robot p (y , -x ) c
turn _ _ = undefined

move :: Robot -> Robot
move (Robot (x,y) (dx,dy) c) = Robot (x+dx,y+dy) (dx,dy) c


-- robot gets color, returns the color that should be put down, and the robot's new, moved positon
runRobot :: Robot -> Color -> (Robot, Color)
runRobot (Robot p d c) color = (move $ turn (Robot p d (clearOut c2)) dir , paintcolor)
             where c2 = runComp (addInp c color)
                   [dir, paintcolor] = output c2

paintOneTile :: (Floor, Robot) -> (Floor, Robot)
paintOneTile (f,r) = (M.insert rpos newc' f, r')
        where rpos = robotPos r 
              currentColor = fromMaybe 0 $ M.lookup rpos f
              (r' , newc') = runRobot r currentColor

doTheWholeHull :: Floor -> Robot -> Floor 
doTheWholeHull f r = fst . head . dropWhile(\x -> Halted /= (state . robotC . snd $ x) ) $ iterate paintOneTile (f , r)


run :: IO ()
run = do
   putStrLn "Day11 Space Police (Robot Painting)"
   comp <-  readInp "input11.txt" >>= \s ->return $ initComp (parseString s:: UV.Vector Int ) []
   putStr " Part1: "
   part1 comp >>= assertInt 2720
   putStr " Part2: "
   part2 comp 
part1 :: PComputer  -> IO Int
part1 c = return $ length $ doTheWholeHull M.empty (initRobot c) 

part2 :: PComputer  -> IO ()
part2 c = do
    let finishedFloor = doTheWholeHull (M.singleton (0,0) 1) (initRobot c) 
        q = mapBounds $ M.keys finishedFloor
        ((x1,y1),(x2,y2))  = q 
        s = map (\k -> intDispl $ fromMaybe 0 (M.lookup k finishedFloor)) ( flip (,) <$> reverse [y1..y2] <*> [x1..x2])

    putStrLn ""
    putStrLn $ unlines . chunksOf (1+x2-x1) $ s

--
