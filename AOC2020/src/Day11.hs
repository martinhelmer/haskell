import Data.Array
import Distribution.Simple.Utils (xargs)
import Distribution.Compat.Graph (neighbors)

data Seat = Empty | Occupied | Floor  deriving (Eq, Show)

type Aix = (Int, Int)


charToSeat 'L' = Empty
charToSeat '#' = Occupied
charToSeat '.' = Floor
charToSeat _ = undefined

seatToChar x = case x of
    Empty -> 'L'
    Occupied -> '#'
    Floor -> '.'

flipSeat :: Seat -> Int -> Seat
flipSeat Empty nOcc= if nOcc == 0 then  Occupied else Empty
flipSeat Floor _ = Floor
flipSeat Occupied nOcc = if nOcc >= 4 then Empty else Occupied


flipSeat2 :: Array Aix Seat -> [Seat] -> Seat -> Seat
flipSeat2 _   _         Floor = Floor
flipSeat2 arr neighbors Empty = if Occupied `elem` neighbors then Empty else Occupied
flipSeat2 arr neighbors Occupied = if length(take 5 (filter (==Occupied) neighbors)) == 5 then Empty else Occupied


neighSeats ::  Array Aix Seat -> Aix -> [Seat]
neighSeats a ix = map (a !) (neighIxs (bounds a) ix)
    where neighIxs :: (Aix, Aix) -> Aix -> [Aix]
          neighIxs ((i,j),(k,l)) (a,b) =
                        [ (x,y) | x <- [(a-1)..(a+1)] , y<-[(b-1)..(b+1)] ,
                                x >= i && x <= k && y >= j && y <= l && (x/=a || y /= b) ]

dirs :: [(Int, Int)]
dirs =  [ (x,y) | x <- [-1..1] , y <- [-1..1] , (x /=0 || y /=0 )]

neighSeats2 :: Array Aix Seat -> Aix -> [Seat]
neighSeats2 a ix =  map (findSeat a ix) dirs

findSeat :: Array Aix Seat -> Aix -> (Int, Int)-> Seat
findSeat arr ix dir | not $ isValidIx (bounds arr) nextIx= Floor
                     | nextSeat == Floor = findSeat arr nextIx dir
                     | otherwise = nextSeat
    where nextIx = tSum ix dir
          nextSeat :: Seat
          nextSeat = arr ! nextIx
          isValidIx ((i,j),(k,l)) (a,b) =  i <= a && a <= k && j <= b && b <= l

tSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tSum (a,b) (c,d) = (a+c, b+d)


-- nextArr :: Array Aix Seat -> Array Aix Seat
-- nextArr arr = listArray (bounds arr) (map go $ indices arr)
--     where go i =  if value == Floor then Floor else flipSeat value $ numOcc $ neighSeats arr i
--           value = arr ! i
--           numOcc = length . filter (==Occupied)

nextArr :: Array Aix Seat -> (Array Aix Seat -> Aix -> [Seat]) -> Array Aix Seat
nextArr arr f= listArray (bounds arr) newValues 
    where newValues = map go $ indices arr
          go i =  flipSeat2 arr (f arr i) (arr ! i)



mkArr :: [[a]] -> Array Aix a
mkArr l = listArray dims $ concat l
    where dims = ((1,1),(rows, cols))
          cols = length (head l)
          rows = length l

str2floor :: [String] -> [[Seat]]
str2floor = map (map charToSeat)

testArr =
    ["#.LL.L#.##",
     "#LL..LL.L#",
     "#LL.#LL.L#"]

arrFromStr  = mkArr . map (map charToSeat)

strFromArr :: Array Aix Seat -> [[Char]]
strFromArr a =  splitup (cols $ bounds a) (map seatToChar $ elems a)
    where cols (_,(_,d)) = d

          splitup :: Int -> [a] -> [[a]]
          splitup n [] = []
          splitup n l = (take n l ): (splitup n (drop n l))

flupp :: [String] -> IO()
flupp [] = putStrLn " "
flupp (x:xs) = do
                putStrLn  x
                flupp xs


doit :: Int -> Array Aix Seat -> IO()
doit n a = do
    -- putStrLn ""
    -- putStrLn $ show n 
    -- flupp $ strFromArr a
    let b = nextArr a neighSeats2
    if b /= a then
        doit (n+1) b
    else
        do
            flupp $ strFromArr b
            print $ length $ filter (==Occupied) $ elems b

main :: IO ()
main = do
    l <- getContents
    let boat = arrFromStr (lines l)
    doit 0 boat




