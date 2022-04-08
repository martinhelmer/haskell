
module Counter where 
    
import qualified Data.Map.Strict as Map

type Counter = Map.Map

empty = Map.empty 


inc :: (Ord k, Num a) => k -> Counter k a -> Counter k a
inc k = Map.insertWith (+) k 1

maximum :: (Ord k, Ord a) => Counter k a -> (k, a)
maximum c = Map.foldrWithKey (\k a (mk,mv) -> if mv > a then (mk,mv) else (k,a)) (head $ Map.assocs c) c

