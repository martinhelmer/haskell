
-- input :: [Int]
import qualified Data.IntMap.Lazy as Map

input = [0,1,4,13,15,12,16]
getNth :: Int -> Map.IntMap Int -> Int -> Int -> Int
getNth n memo last pos
    | pos == n = last
    | otherwise =
        let updatedMemo = Map.insert last pos memo
            diff = case Map.lookup last memo of
                    Nothing -> 0
                    Just i -> pos - i
        in getNth n updatedMemo diff (pos + 1)

solve :: Int
solve = let initMap = Map.fromList $ zip input [1..]
            in getNth 30000000 initMap 0 (length input + 1)


main :: IO ()
main = print $ solve
