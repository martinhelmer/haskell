-- https://www.seas.upenn.edu/~cis194/spring13/lectures/02-ADTs.html

data Thing = Shoe 
            | Ship
            | SealingWax
            | Cabbage
            | King
    deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship       = False
isSmall King       = False
isSmall _          = True 

-- Beyong Enumerations
data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 :: FailableDouble
ex01 = Failure
ex02 :: FailableDouble
ex02 = OK 3.4
-- Thought exercise: what is the type of OK?

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing
    deriving Show 

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage  

getAge :: Person -> Int
getAge (Person _ a _) = a

-- One final note: type and data constructor names must always start with a capital letter; 
-- variables (including names of functions) must always start with a lowercase letter. 
-- (Otherwise, Haskell parsers would have quite a difficult job figuring out which names represent
-- variables and which represent constructors).

-- Case Expressions

-- ex03 = case "Hello" of
--            []      -> 3
--            ('H':s) -> length s
--            _       -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

data IntList = Empty | Cons Int IntList
    deriving Show 

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

