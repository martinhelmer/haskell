module Day20 where 
    
import Helpers
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Map as Map

type TileID = Int
data Tile = Tile TileID (Int,Int) (Int,Int) (Int,Int) (Int,Int) [String] deriving (Eq, Show)

mkTile tID xs = Tile tID (top xs) (right xs) (bottom xs) (left xs) xs
    where
        left = edgeInts . map head
        right = edgeInts . map last
        top = edgeInts . head
        bottom = edgeInts . last

        edgeInts :: String -> (Int,Int)
        edgeInts s = tf (read . go , read . go . reverse) s
            where go = map (\x -> if x =='#' then '1' else '0' )

tileID (Tile t _ _ _ _ _) = t
tileTop (Tile _ t _ _ _ _ ) = t
tileRight (Tile _ _ r _ _ _ ) = r
tileBottom (Tile _ _ _ b _ _ ) = b
tileLeft (Tile _ _ _ _ l _ ) = l

rotL ::  Int -> Tile -> (Int, Int)
rotL 1 = tf (fst . tileRight , snd . tileTop)
rotL 2 = tf (snd . tileBottom, snd . tileRight)
rotL 3 = tf (snd . tileLeft, fst . tileBottom)
rotL 4 = tf (fst . tileTop, fst . tileLeft)      -- symmetry check
rotL _ = undefined

tf (f1, f2) t = (f1 t,f2 t)

flipTile (Tile tID _ _ _ _ txt) =  mkTile tID $  reverse txt

rotateLeft :: Int ->Tile -> Tile
rotateLeft n (Tile tID _ _ _ _ xs) = mkTile tID $ iterate (reverse . transpose ) xs !! n

test1 = ["Tile 2311:",
         "..##.#..#.",
        "##..#.....",
        "#...##..#.",
        "####.#...#",
        "##.##.###.",
        "##...#.###",
        ".#.#.#..##",
        "..#....#..",
        "###...#.#.",
        "..###..###"]

parseTile :: [String] -> Tile
parseTile (x:xs) = mkTile (read . init . last $ words x ) xs

parseTiles :: [String] -> [Tile]
parseTiles s = map parseTile $ splitBy "" s

tileEdges :: Tile -> [Int]
tileEdges (Tile _ (a,b) (c,d) (e,f) (g,h) _ ) = [a,b,c,d,e,f,g,h]

allEdges t = Set.fromList $ concatMap tileEdges t

tt tiles t1= (tileID t1 , Set.fromList (tileEdges t1) `Set.intersection` allEdges (filter (/=t1) tiles))

-- part 2 

tileMatch :: Int -> Int -> Tile -> Maybe Tile
tileMatch top left tile =  if isNothing tmr then tileMatchRot top left $ flipTile tile else tmr
        where tmr = tileMatchRot top left tile

tileMatchRot :: Int -> Int -> Tile -> Maybe Tile
tileMatchRot  (-1) left tile | left == fst (tileLeft tile) = Just tile
                             | left == snd (rotL 1 tile) = Just $ rotateLeft 1 tile
                             | left == snd (rotL 2 tile) = Just $ rotateLeft 2 tile
                             | left == snd (rotL 3 tile) = Just $ rotateLeft 3 tile
                             | otherwise = Nothing
tileMatchRot  top (-1) tile  | top == fst (tileTop tile) = Just tile
                             | top == fst (rotL 1 tile) = Just $ rotateLeft 1 tile
                             | top == fst (rotL 2 tile) = Just $ rotateLeft 2 tile
                             | top == fst (rotL 3 tile) = Just $ rotateLeft 3 tile
                             | otherwise = Nothing
tileMatchRot  top left tile  | top == fst (tileTop tile) && left == fst (tileLeft tile) = Just tile
                             | (top , left) == rotL 1 tile = Just $ rotateLeft 1 tile
                             | (top , left) == rotL 2 tile = Just $ rotateLeft 2 tile
                             | (top , left) == rotL 3 tile = Just $ rotateLeft 3 tile
                             | otherwise = Nothing

type TileArray = Array (Int,Int) (Maybe Tile)
type TileMap = Map.Map TileID Tile

data State = State TileArray TileMap  deriving Show
stateArr (State a _) = a
stateMap (State _ m) = m

getInitState :: [Tile] -> State
getInitState tiles = State arr $ Map.delete (tileID first) m
    where m = Map.fromList $ zip (map tileID tiles) tiles
          arr = listArray ((1,1),(dim,dim)) $ Just first:repeat Nothing
          first = fitFirst m (getFirstInfo tiles)
          dim = round $ sqrt (fromIntegral (length tiles) ::Float)

getFirstInfo :: [Tile] -> (TileID, Set.Set Int)
getFirstInfo tiles = head . filter (\x -> length (snd x) == 4) $ map (tt tiles) tiles

fitFirst :: TileMap -> (TileID, Set.Set Int) -> Tile
fitFirst m (tid,set) | fits t = t
                         | fits (rotateLeft 1 t) = rotateLeft 1 t
                         | fits (rotateLeft 2 t) = rotateLeft 2 t
                         | fits (rotateLeft 3 t) = rotateLeft 3 t
                         | otherwise = undefined
    where   t = fromJust $ Map.lookup tid m
            fits t = ttoL (tileRight t) (tileBottom t) == set
            ttoL (a,b) (c,d) = Set.fromList [a,b,c,d]

findNext :: State -> State
findNext (State a m) = State (a // [((r,c),theTile)]) (Map.delete (tileID $ fromJust theTile) m)
    where (r,c) = fst . head $ dropWhile (isJust . snd) (assocs a)
          leftFit = if c == 1 then -1 else fst $ tileRight $ fromJust (a ! (r,c-1) )
          upFit   = if r == 1 then -1 else fst $ tileBottom $ fromJust (a ! (r-1,c) )
          theTile = head . dropWhile (== Nothing ) $ map (\(id, tile) -> tileMatch upFit leftFit tile) $   Map.assocs m

tileImage :: Tile -> [String]
tileImage (Tile _ _ _ _ _ xs) = map (init . tail) (init $ tail xs)

mkImage :: TileArray -> [String]
mkImage a = concatMap mkImageRow . mk2D dim . map fromJust . elems $ a
    where dim = snd . snd . bounds $ a

mkImageRow :: [Tile] -> [String]
mkImageRow tiles =  map concat $ transpose $ map tileImage tiles

mk2D ::Int -> [a] -> [[a]]
mk2D n [] = []
mk2D n xs = take n xs:mk2D n (drop n xs)

monster = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]
monsterMask = map (\((x,y),_) -> (x-1,y-1)) $ filter ((==) '#' . snd) $ zip (ixs . listListDim $ monster)  (concat monster)

type Mask = [(Int, Int)]

isMonster :: Array2D Char -> Mask -> Bool 
isMonster img = all (\(r,c) -> img ! (r,c) == '#')

allOverlays :: Array2D Char -> Mask -> [Mask]
allOverlays a m = mapMaybe overlayFromM (indices a)
    where overlayFromM :: (Int,Int) -> Maybe  [(Int, Int)]
          overlayFromM (c, r) =  maybeList $ map (maybeAddIxs a (c,r)) m

maybeAddIxs :: Array2D a -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
maybeAddIxs a (c,r) (c', r')= if inRange (bounds a) s then Just s else Nothing 
    where s = (c+c', r+r')

maybeList ::[Maybe a]-> Maybe [a]
maybeList [] = Just []
maybeList l = foldl go (Just []) l
  where go Nothing _ = Nothing 
        go _ Nothing  = Nothing 
        go (Just l) (Just e) = Just (e:l)


main :: IO ()
main = do
    l <- getContents
    let tiles = parseTiles $ lines l
    -- part 1 
    print $ product . map fst . filter (\x -> length (snd x) == 4) $ map (tt tiles) tiles
    -- part 2 
    let orderedTiles = stateArr $ iterate findNext (getInitState tiles) !! (length tiles -1)
    let image = mkImage orderedTiles
    let imArr = listList2array id image
    putStrLn $ intercalate "\n" image
    print $ length .  filter (isMonster imArr) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray . rLeftArray $ imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray . rLeftArray. rLeftArray $ imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (flipArray imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray . flipArray $ imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray . rLeftArray. flipArray  $ imArr)) $ allOverlays imArr monsterMask
    print $ length .  filter (isMonster (rLeftArray . rLeftArray . rLeftArray. flipArray  $ imArr)) $ allOverlays imArr monsterMask
    let hashtags = length . filter (=='#') $ elems imArr
    print $ hashtags - (18*length monsterMask)