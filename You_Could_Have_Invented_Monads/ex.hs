import Data.Complex

f' :: Num a => a -> (a, String)
f' v = (2*v, "f was called")
g' :: Num a => a -> (a, String)
g' v = (v*v, "g was called")


bind1 :: (a1 -> (a2, String)) -> (a1, String) -> (a2, String)
bind1 f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)


unit q = (q, "")


sqrt' :: Complex Float -> [Complex Float]
sqrt' = undefined

cbrt' :: Complex Float -> [Complex Float]
cbrt' = undefined

bind2 :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
bind2  =  concatMap

unit2 = flip (:) []

-- f * g = bind f . g
-- lift f = unit . f
-- (f * unit ) x = (bind f . unit) x = bind f . unit x = bind f [x] = concatMap f [x]  = 
-- lift f * lift g = bind (unit . f) . (unit . g) = 
-- lift (f.g) = unit . f.g = 

-- "We now must work out how to compose two randomized functions, f and g. 
-- The pair that the function that is returned by g returns needs to be 
-- decomposed and passed in as input to f. So we can give this signature for bind:"

bind :: (a -> c -> (b,c)) -> (c -> (a,c)) -> (c -> (b,c))
bind f x seed = let (x',seed') = x seed in f x' seed'
