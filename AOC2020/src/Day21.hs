module Day21 where 
    
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import qualified Data.Map as Map 
import qualified Data.Set as Set 
import Data.List 

ex1 = ["xcsxh lqbcg bdfmncs qbvxrbs nkcl skrxt hxgdp ndnlm vhbjp nxcsxc flg nxbn kgpbh (contains soy, wheat, peanuts)",
       "zjlz jdcbc nkcl rpqss gqkf lsll flg zbhp ltjt jmt fzckq rnln mknsj ccn qkbm chjqncr lfgh bsdc bzlnnv bbtn fsr skrxt kvhbj hpsrqbl kgdvd lpm dvjrrkv ptcnh tnvtm sgdlhb rfcl gtdb tvgcv rpgrk mkmcq hvrrhc qjktms bsgnsc zsggkm pvnr shc zcxthb lhcm bqjqf fzl rnsqjkq lqgn brhmkh jkgf mv xntpsg mgbv fdjrml dgdn mhqxk ktsnt xcljh qfgbx tkvddn xjksh lqbcg pkrdz brjcp bbxsn (contains fish, peanuts)"]
row1 = head ex1 

parseRow :: String -> Food 
parseRow s = (ingredients, allergens)
    where (ingredients, _:allergens') = span (/="(contains") $ words s 
          allergens = map init allergens'

type AllergenMap = Map.Map String [String]
type Ingredients = [String]
type Food = ([String],[String])

data FoodInfo = FoodInfo Ingredients AllergenMap deriving Show 
initFoodInfo = FoodInfo [] Map.empty 

addFood :: FoodInfo -> Food -> FoodInfo
addFood (FoodInfo ins allMap) (ingredients, allergens) = 
    FoodInfo (ins ++ ingredients) $ foldr (updateAllergen ingredients) allMap allergens
        where updateAllergen ingredients allergen = Map.insertWith intersect allergen ingredients 

foldFood :: [String] -> FoodInfo
foldFood l = foldl' addFood initFoodInfo $ map parseRow l

safeItems :: FoodInfo -> [String]
safeItems (FoodInfo ins m) = filter (\i -> i `notElem` (nub . concat $ Map.elems m))  ins


main :: IO ()
main = do
    l <- getContents
    print $ foldFood $ lines l 
    print $ safeItems $ foldFood $ lines l 
    print $ length $ safeItems $ foldFood $ lines l 
