module Ch8 where

cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"


frappe :: String -> String
frappe = flippy "haha"

-- ...

-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 n | n > 100 = n - 10
       | otherwise = mc91(mc91(n+11))
    

main :: IO()
main = do
    print $ frappe (appedCatty "2") == "woops mrow 2 mrow haha"
    print $ appedCatty (frappe "blue") == "woops mrow blue mrow haha"
    print $ cattyConny (frappe "pink")
                        (cattyConny "green" (appedCatty "blue")) ==
                            "pink mrow haha mrow green mrow woops mrow blue"
    putStr "McCarthy 91 function:"
    print $ map mc91 [95..110] == [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
