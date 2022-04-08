{-# LANGUAGE FlexibleInstances #-}
module JoinList where 
import Sized 
import Scrabble 
import Data.List 
import Data.Monoid
import Buffer

testList = Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) 'y')
                (Append (Size 2)
                   (Single (Size 1) 'e')
                   (Single (Size 1) 'a')))
            (Single (Size 1) 'h')

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
(+++) Empty l = l
(+++) l Empty = l
(+++) l l'  = Append (tag l `mappend` tag l') l l'

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m 
tag (Append m _ _) = m
tag Empty = undefined 

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing 
indexJ i _ | i < 0 = Nothing 
indexJ i l@(Single _ a) | i == 0 = Just a
                        | otherwise = Nothing 
indexJ i (Append b l r) | getSize (size b) < i = Nothing 
                        | lSize > i = indexJ i l 
                        | otherwise = indexJ (i-lSize) r
        where lSize = getSize (size $ tag l)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ i Empty = Empty 
dropJ i (Single _ _) = Empty 
dropJ i (Append b l r) | getSize (size b) < i = Empty
                       | lSize < i = dropJ (i - lSize) r 
                       | otherwise = dropJ i l +++ r
                       where lSize = getSize (size $ tag l)
                           

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 l = Empty 
takeJ n Empty = Empty
takeJ n s@(Single _ _) = s 
takeJ n s@(Append b l r) | getSize (size b) <= n = s
                         | lSize >= n = takeJ n l
                         | otherwise = l +++ takeJ (n-lSize) r
                         where lSize = getSize (size $ tag l)

--

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

sizeScoreLine :: String -> JL
sizeScoreLine s = Single (scoreString s, Size 1) s

buffList :: [String] -> JL
buffList = foldl'(\a x -> a +++ sizeScoreLine x ) Empty

type JL = JoinList (Score,Size) String

instance Buffer JL where
  toString     = unlines . jlToList
  fromString   = buffList . lines 
  line    = indexJ 
  replaceLine n l b = takeJ n b +++ sizeScoreLine l +++ dropJ (n+1) b
  numLines     = getSize . size . tag 
  value        = getScore . fst . tag 
