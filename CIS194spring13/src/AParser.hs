{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# LANGUAGE InstanceSigs #-}

module AParser (Parser, runParser, satisfy, char, posInt) where
import           Control.Applicative

import Data.Char
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap f (Parser g)= Parser q
     where q s= case g s of
             Nothing -> Nothing
             Just (a, s) -> Just (f a, s)


instance Applicative Parser where
  pure  :: a -> Parser a
  pure a = Parser (\s -> Just (a, s))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f1 <*> Parser f2 = Parser (z f1 f2)

z :: (String -> Maybe (a->b, String)) ->
      (String -> Maybe (a, String)) ->
        String -> Maybe (b, String)
z f1 f2 s = if isNothing step1 then Nothing else
             maybe Nothing (\(parsed2,rest2) -> Just( parsed1 parsed2, rest2)) (f2 rest1)
    where step1 = f1 s
          (parsed1,rest1) = fromJust step1

----------------
-- Ex 3
----------------
null1 a = ()
null2 a b = ()

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = null2 <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a b c -> [a, c]) <$> posInt <*> char ' ' <*> posInt

----------------
-- Ex 4
----------------

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) p1 p2 = Parser (f p1 p2)
    where f p1 p2 s = runParser p1 s <|> runParser p2 s

----------------
-- Ex 5
----------------
intOrUppercase :: Parser ()
intOrUppercase = null1 <$> posInt <|> null1 <$> satisfy isUpper

