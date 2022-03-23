
import Helpers
import Data.List 

test = ["class: 1-3 or 5-7",
        "row: 6-11 or 33-44",
        "seat: 13-40 or 45-50",
        "",
        "your ticket:",
        "7,1,14",
        "",
        "nearby tickets:",
        "7,3,47",
        "40,4,50",
        "55,2,20",
        "38,6,12"]

type Range = (Int, Int)
data Field = Field String Range Range deriving (Eq, Show)
type Fields = [Field]
type Ticket = [Int]
type TheirTickets = [Ticket]
data Stuff = Stuff Fields Ticket TheirTickets deriving Show


isInvalid :: Int -> Fields -> Bool
isInvalid i = not . isValid i

isValid :: Int -> Fields  -> Bool
isValid i = any (inRange i)

inRange :: Int -> Field -> Bool
inRange i (Field _ r1 r2) = r r1 || r r2
      where r:: Range -> Bool
            r (a,b) = (a <= i) && (i <= b)

parseStuff :: [String] -> Stuff
parseStuff x = Stuff (map parseField (s !! 0)) (parseTicket $ last (s !! 1)) $ map parseTicket $ tail (s !! 2)
        where s = splitBy "" x

parseField :: String -> Field
parseField x =  Field name (parseRange (w !! 0)) (parseRange (w !! 2))
        where (name, rest) = span (/=':') x
              w = words . tail $ rest
              parseRange :: String -> (Int , Int)
              parseRange s = (read $ spl !! 0 , read $ spl !! 1)
                    where spl = splitBy '-' s

parseTicket :: String -> Ticket
parseTicket s = read $ "[" ++ s ++ "]"

invalids :: Stuff -> [Int]
invalids (Stuff fields _ tickets ) = filter (`isInvalid` fields) . concat $ tickets

-- Part 2
myticket :: Stuff -> Ticket  
myticket (Stuff _ t _) = t 

isDepField :: Field -> Bool
isDepField (Field name _ _ ) = "departu" `isPrefixOf` name 

validTickets :: Stuff -> TheirTickets
validTickets (Stuff fields _ tickets ) = filter (all (`isValid` fields)) tickets

getTickets (Stuff _ _ t) = t 

type Solution = [Fields]

getStartSol :: Stuff -> Solution
getStartSol (Stuff f _ _) = replicate (length f) f

matchingFields :: Int -> Fields -> Fields
matchingFields i = filter ( inRange i )

filterSolution :: Solution -> Ticket -> Solution 
filterSolution s t = map (uncurry matchingFields) $ zip t s 

singleFields :: Solution -> Fields 
singleFields = concat . filter ((1 ==) . length) 

reduceSol :: Solution -> Solution 
reduceSol s = if s == nextRed then s else reduceSol nextRed
    where nextRed = map (\x -> if length x == 1 then x else filter ( `notElem` singleFields s ) x) s

sol :: Stuff -> Fields
sol stuff = concat . reduceSol . foldl' filterSolution (getStartSol stuff) . validTickets $ stuff


ss = filterSolution (getStartSol . parseStuff $ test) [7,3,47] 

main :: IO ()
main = do
    l <- getContents
    let stuff = parseStuff . lines $ l
    print $ sum . invalids $ stuff
    print $ sol stuff
    print $ product . map fst . filter (isDepField . snd)  $ zip (myticket stuff) (sol stuff)





    



