module PoemLines where
firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen


shouldEqual =
     [ "Tyger Tyger, burning bright"
     , "In the forests of the night"
     , "What immortal hand or eye"
     , "Could frame thy fearful symmetry?"
     ]

mySplit :: Char -> String -> [String]
mySplit c s| skipC s == "" = []
           | otherwise  = takeWhile (/=c) (skipC s) : mySplit c (dropWhile (/= c) $ skipC s )
    where skipC = dropWhile (== c)

myWords = mySplit ' '
myLines = mySplit '\n' 

main = do
    print $ myWords "Hello Wordl"
    putStrLn sentences
    print $
     "Are they equal? "
     ++ show (myLines sentences == shouldEqual)
     