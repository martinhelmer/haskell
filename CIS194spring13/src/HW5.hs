{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import ExprT
import Parser
import GHC.Platform (IntegerLibrary(IntegerGMP))
import StackVM
import qualified Data.Map as M
import Data.Maybe (fromJust)


-- ex 1
exercise1 = do
    print $ eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20

{- data ExprT = Lit Integer
| Add ExprT ExprT
| Mul ExprT ExprT
deriving (Show, Eq)
-}


eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- ex 2
evalStr :: String -> Maybe Integer
evalStr = f . parseExp ExprT.Lit ExprT.Add ExprT.Mul 
    where 
        f :: Maybe ExprT -> Maybe Integer
        f Nothing = Nothing 
        f (Just exp) = Just $ eval exp

-- ex3
exercise3 = do
    print $ (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) ==  ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)

class Expr a where
    lit :: Integer  -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit a = ExprT.Lit a
    add a b = ExprT.Add a b
    mul a b = ExprT.Mul a b 

instance Expr Integer where
    lit a = a
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit a = a > 0
    add a b = a || b
    mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit a = MinMax a
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b) 


reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- ex 5
instance Expr Program where
    lit a = [PushI a]
    add a b = a ++ b ++ [StackVM.Add] 
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


-- ex 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
            | Add VarExprT VarExprT
            | Mul VarExprT VarExprT
            | Var String 
            deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = Main.Lit
    add a b = Main.Add a b
    mul a b = Main.Mul a b 

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- This took a LONG time
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a = const $ Just a
    add a b = sf a b 
    mul a b = mf a b 


sf :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
sf a b m = fmap sum $ sequence [a m , b m ]

mf :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
mf a b m = fmap product $ sequence [a m , b m ]


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

exercise6 = do
   print $ withVars [("x", 6)] ( add (lit 3) (var "x")) == Just 9
   print $ withVars [("x", 6)] ( add (lit 3) (var "y")) == Nothing
   print $ withVars [("x", 6), ("y", 3)] ( mul (var "x") (add (var "y") (var "x"))) == Just 54


main = do
    exercise1
    exercise3
    exercise6


