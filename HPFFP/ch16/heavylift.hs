
a = fmap (+1) $ read "[1]" :: [Int]
a' = (+1) <$> read "[1]" :: [Int]

testA = a == [2]


b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
testB = b == Just ["Hi,lol","Hellolol"]



c = (*2) <$> (\x -> x - 2)
testC = c 1 == -2 

d =
    ((return '1' ++) . show) <$> (\x -> [x, 1..3])

testD = d 0 == "1[0,1,2,3]"

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi in fmap (*3) changed

