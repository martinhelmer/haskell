{-# LANGUAGE FlexibleInstances #-}

--  exercise 1
fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

exercise1 = do
    print $ fib 26
    print $ take 27 fibs1

-- exercise 2
fibs2 :: [Integer]
fibs2 = fibl 0 1 
    where  
        fibl a b = a:fibl b (a+b)

exercise2 = do
    print $ take 27 fibs2


-- exercise 3
exercise3 = do
    print $ show $ streamRepeat 5
    
-- this took a while
data Stream a = Head a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Head a b)= a:streamToList b 

instance Show a => Show (Stream a) where
    show a = show $ take 30 $ streamToList a

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Head a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Head a s) = Head (f a) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Head a $ streamFromSeed f (f a)  

-- exercise 5
exercise5 = do
    print $ show nats
    print $ show ruler

nats :: Stream Integer 
nats = streamFromSeed (+ 1) 0 

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Head a s) (Head b s2) = Head a $ Head b (interleaveStreams s s2)

ruler :: Stream Integer
ruler = nextRuler 0 
    where 
        nextRuler :: Integer -> Stream Integer 
        nextRuler a = Head a $ interleaveStreams (nextRuler $ a+1) $ streamRepeat a


zeros :: Stream Integer
zeros = streamRepeat 0

ones :: Stream Integer
ones = streamRepeat 1

x :: Stream Integer 
x = Head 0 $ Head 1 zeros

instance Num (Stream Integer) where
    fromInteger n = Head n zeros
    negate = streamMap (0-)
    (Head a s) + (Head b s2) = Head (a+b) $ s + s2 
    (Head a s) - (Head b s2) = Head (a-b) $ s - s2 
    a * b = streamMul a b
        where streamMul :: Stream Integer -> Stream Integer -> Stream Integer
              streamMul (Head a0 a') b@(Head b0 b') = Head (a0 * b0) $ streamMap (a0*) b' + streamMul a' b 

instance Fractional  (Stream Integer) where
    a / b = streamDiv a b 
        where streamDiv :: Stream Integer -> Stream Integer -> Stream Integer
              streamDiv a@(Head a0 a') b@(Head b0 b') = 
                  Head (a0 * b0) $ 
                  streamMap (`div` b0) $ a' - streamDiv a b * b'
                 

fibs3 :: Stream Integer
fibs3 = x / ( 1 - x - x^2)

main = do
    exercise1
    exercise2
    exercise3

