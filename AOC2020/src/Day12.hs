import Data.List (foldl')

data Position =
        Position Longitude Lat Heading deriving Show

class BoatPos a where
  mDist :: a  -> Int
  startPos :: a

instance BoatPos Position where
    mDist (Position long lat _) = abs long + abs lat 
    startPos = Position 0 0 East

type Longitude = Int
type Lat = Int
data Heading = North | East | South | West deriving (Show, Enum, Bounded, Eq)

data Instruction =
    Move Heading Int
    | Turn Direction Degrees
    | Forward Int

type Degrees = Int
data Direction = R | L

testInput = ["F10","N3","F7","R90","F11"]

-- part 2 
data Position2 = Position2 Longitude Lat WaypLong WaypLat deriving Show

type WaypLong = Int
type WaypLat = Int

instance BoatPos Position2 where
    mDist (Position2 long lat _ _) = abs long + abs lat 
    startPos = Position2 0 0 10 1


doMove2 :: Position2 -> Instruction -> Position2
doMove2 (Position2 long lat waypLong waypLat) (Forward v) =  Position2 (long+v*waypLong) (lat+v*waypLat) waypLong waypLat
doMove2 p@(Position2 long lat waypLong waypLat) (Move h v) = case h of
                                                North -> Position2 long lat waypLong (waypLat+v)
                                                South -> doMove2 p $ Move North (-v)
                                                East ->  Position2 long lat (waypLong+v) waypLat
                                                West ->  doMove2 p $ Move East (-v)
doMove2 p@(Position2 long lat waypLong waypLat) (Turn dir deg) = turn dir (deg `div` 90)
        where turn _ 0 = p
              turn L n = turn R (4-n)
              turn R n = Position2 long lat (fst rotCoord) (snd rotCoord)
                where rotCoord = iterate (\(x,y)->(y,-x)) (waypLong, waypLat) !! n  -- rotate right n times
-- ^^ part 2

turn :: Heading -> Direction -> Degrees -> Heading
turn h _ 0 = h
turn h L d = turn h R (360-d)
turn h R d = rotations h !! (n-1)
    where n = d `div` 90
          rotations :: Heading -> [Heading]
          rotations h = next : rotations next
             where next = if h == (maxBound::Heading) then minBound :: Heading else succ h

doMove :: Position -> Instruction -> Position
doMove (Position long lat head) (Forward v) =  doMove (Position long lat head) (Move head v)
doMove (Position long lat head) (Turn dir deg) = Position long lat (turn head dir deg)
doMove (Position long lat head) (Move h v) = case h of
                                                North -> Position long (lat+v) head
                                                East -> Position (long+v) lat head
                                                South -> Position long (lat-v) head
                                                West -> Position (long-v) lat head


parseInstruction :: String -> Instruction
parseInstruction [] = undefined 
parseInstruction (x:xs) = case x of
        'F' -> Forward (read xs)
        'L' -> Turn L (read xs)
        'R' -> Turn R (read xs)
        'N' -> Move North (read xs)
        'S' -> Move South (read xs)
        'W' -> Move West (read xs)
        'E' -> Move East (read xs)
        _ -> undefined

followInstructions :: (BoatPos p) => (p -> Instruction -> p) -> [String] -> p
followInstructions mover = foldl' (\p i -> mover p (parseInstruction i)) startPos

main :: IO ()
main = do
    l <- getContents
    putStr "Part 1 :"
    print $ mDist $ followInstructions doMove (lines l)
    putStr "Part 2 :"
    print $ mDist $ followInstructions doMove2 (lines l)