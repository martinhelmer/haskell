{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Maybe
import Data.Char
import Exception (gonException)
------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- smuck x (Just (a,b)) = Just (x:a,b)

-- zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = Parser go
--                where go s = maybe (Just ([], s))
--                              (\(x,s')-> smuck x (go s' ))
--                              $ runParser p s

-- oneOrMore :: Parser a -> Parser [a]
-- oneOrMore p = Parser go
--               where go s= maybe Nothing
--                                 (\(x,s')-> smuck x (runParser (zeroOrMore p) s'))
--                                 $ runParser p s

-- Hint: To parse one or more occurrences
-- of p, run p once and then parse zero or
-- more occurrences of p. To parse zero or
-- more occurrences of p, try parsing one
-- or more; if that fails, return the empty
-- list

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


examples = [
  "5",
  "foo3",
  "(bar (foo) 3 5 874)",
  "(((lambda x (lambda y (plus x y))) 3) 5)",
  "( lots of ( spaces in ) this ( one ) )"]

atomP :: Parser SExpr
atomP = A <$> (spaces *> (N <$> posInt <|> I <$> ident) <* spaces)

combP :: Parser SExpr
combP = Comb <$> (spaces *> satisfy (=='(') *> oneOrMore parseSExpr  <* satisfy (==')') <* spaces)

parseSExpr :: Parser SExpr
parseSExpr = atomP <|> combP



sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (ma:mas) =
  ma >>= \a -> (sequence' mas >>= \as -> return (a:as))

replicateM :: Monad m => Integer -> m a -> m [a]
replicateM n m = sequence (replicate n m)

parseFile :: Parser [[Integer]]
parseFile = many parseLine

parseLine :: Parser [Integer]
parseLine = posInt >>= \i -> replicateM i posInt
