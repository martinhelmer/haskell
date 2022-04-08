module Party where

import Employee
import Data.List
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons  e@Emp {empName = n,empFun = f} (GL l funSum )= GL (e:l) (f+funSum)


instance Semigroup GuestList where
  (GL e f)  <>  b  = foldr  glCons b e

instance Monoid GuestList where
  mempty  = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

instance Monoid Int where
  mempty = 0
  -- equivalently: mappend a b = a + b
  mappend = (+)

instance Semigroup Int where
  (<>) = mappend

testInts :: Tree Int
testInts = Node 1
            [Node 10 []
            ,Node 100
              [Node 1000 []]
            ]

testS :: Tree String
testS = Node "T"
            [Node "A"
              [ Node "a1" []
              , Node "a2" []
              ]
            ,Node "B"
              [Node "b1" []]
            ]
--       
treeFoldl' ::(b -> a -> b) -> b -> Tree a -> b
treeFoldl' f acc (Node v []) = f acc v
treeFoldl' f acc (Node v l) = f (foldl (treeFoldl' f) acc l) v

treeFoldl :: (a -> [b] -> b) -> b -> Tree a -> b
treeFoldl f acc (Node e l) = f e $ map (treeFoldl f acc) l

--                                                    with boss, without boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l = (foldBoss, foldNoBoss)
  where foldBoss = glCons e (mconcat . map snd $ l)
        foldNoBoss = mconcat . map (uncurry moreFun) $ l

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node emp []) = (glCons emp mempty , mempty)
maxFun' (Node emp l) = nextLevel emp $ map maxFun'  l

maxFun = uncurry moreFun . treeFoldl nextLevel (mempty , mempty)

gl2str (GL emps fun )= "Total fun:"++show fun ++ "\n" ++ unlines (map empName emps)

s2output :: String -> IO ()
s2output s =  putStrLn . gl2str . maxFun $ read s

main :: IO()
main = readFile "company.txt" >>= s2output