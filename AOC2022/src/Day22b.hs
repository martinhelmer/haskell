{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Day22b where

import Text.RawString.QQ ( r )
import Data.List ( foldl', transpose ) 
import AOCHelper ( assertIt, readInp, parseIntoArray, draw2dchararr)
import Data.Maybe ( fromJust, mapMaybe, isJust )
import qualified Data.Map as M
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq, (><))
import Text.ParserCombinators.ReadP
import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( isDigit, isAlpha ) 
import Text.Read (readMaybe)
import  Data.List.Split (splitOn)
import qualified Data.Array as A 
import qualified Data.Vector as V 
import Debug.Trace (trace)
import Prelude hiding (Right, Left)

data Rot = None | Left | Right | Upside deriving (Show)

rotleft None = Left 
rotleft Left = Upside 
rotleft Right = None 
rotleft Upside = Right 

rotright None = Right 
rotright Left = None 
rotright Right = Upside 
rotright Upside = Left 

data Face = Face {name::Char, rot::Rot, ln::(Char, Rot), rn::(Char, Rot), un::(Char, Rot), dn::(Char,Rot) } deriving (Show)

a = Face 'a' None (' ',None) ('b',None) (' ',None) (' ',None)
b = Face 'b' None ('a',None) ('c',None) (' ',None) (' ',None)
c = Face 'c' None ('b',None) (' ',None) ('d',None) ('e',None)
d = Face 'd' None (' ',None) (' ',None) (' ',None) ('c',None)
e = Face 'e' None (' ',None) ('f',None) ('c',None) (' ',None)
f = Face 'f' None ('e',None) (' ',None) (' ',None) (' ',None)


m = M.fromList   $   map (\f -> (name f,f))   [a,b,c, d, e, f]
lm k = fromJust $ M.lookup k m

leftrot :: Int -> Face -> Face
leftrot i f@(Face n rot (l,lr) (r,rr) (u,ur) (d,dr)) =(Face n (rotleft rot) (u,rotleft ur) (d,rotleft dr) (r,rotleft rr) (l,rotleft rr) )

rightrot :: Int -> Face -> Face 
rightrot i f@(Face n rot (l,lr) (r,rr) (u,ur) (d,dr)) =(Face n (rotright rot) (d, rotright dr) (u,rotright ur) (l,rotright lr) (r,rotright rr) )

upneigh = nn un leftneigh rightneigh upneigh 
downneigh = nn dn rightneigh leftneigh downneigh
rightneigh = nn rn upneigh downneigh rightneigh
leftneigh = nn ln downneigh upneigh leftneigh

nn df tn bn fn n f | fst (df f) /= ' ' = Just (rfix (lm (fst(df f))) )
                   | n == 0 = Nothing 
                   | otherwise =  if isJust a then a else b         
                where a = rightrot n <$> (tn (n-1) f >>= fn (n-1))
                      b =  leftrot n <$> (bn (n-1) f >>= fn (n-1)) 
                      rfix (Face x rot a b c d ) = Face x (snd (df f)) a b c d 

                      