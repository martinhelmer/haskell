{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- make it possible for DV to be Random value
instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

gg =  mkStdGen 100

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

instance Show Battlefield where
    show b=  show (attackers b) ++ ":" ++ show (defenders b)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
      attackDice <- reverse . sort <$> Control.Monad.replicateM (min 3 $ attackers b) die
      defDice <- reverse . sort <$> Control.Monad.replicateM (min 2 $ defenders b) die
      return  $ consume b attackDice defDice
        where
          consume :: Ord a =>  Battlefield -> [a] -> [a] -> Battlefield
          consume b (a:ax) (d:dx) | a > d = go 0 (-1)
                                  | otherwise = go (-1) 0
              where go x y = consume (Battlefield (attackers b + x) (defenders b + y)) ax dx
          consume b [] _ = b
          consume b _ [] = b


invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d) | a < 2 = return b
                           | d == 0 = return b
                           | otherwise = battle b >>= invade

invadeOften :: Battlefield-> Int -> Rand StdGen [Battlefield]
invadeOften b n = replicateM n (invade b)


successProb :: Battlefield -> Rand StdGen Double
successProb b =  sprob <$> invadeOften b n
  where sprob l =  fromIntegral ( length . filter ((==) 0 . defenders) $ l) / fromIntegral n
        n = 4000 

---
