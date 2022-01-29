{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log 

readMaybe :: String -> Maybe Int
readMaybe s = if all  (`elem`  ['0','1'..'9']) s 
    then Just (read s)
    else Nothing

parseMessage :: String -> LogMessage
parseMessage x = case parseMessage' x of
                    (Unknown _) -> Unknown x
                    m -> m

parseMessage' :: String -> LogMessage 
parseMessage' ('W':' ':xs) = getMsg Warning (getIntArg xs)
parseMessage' ('I':' ':xs) = getMsg Info (getIntArg xs)
parseMessage' ('E':' ':xs) = case getIntArg xs of
                                (Nothing , _ ) -> Unknown ""
                                (Just e, rest) -> getMsg (Error e) (getIntArg rest)
parseMessage' _ = Unknown ""

getMsg :: MessageType -> (Maybe Int, String ) -> LogMessage 
getMsg _ (Nothing, _) = Unknown ""
getMsg typ (Just ts, s) = LogMessage typ ts s

getIntArg :: String -> (Maybe Int, String)
getIntArg s = case span (/=' ') s of
                (i,t) -> (readMaybe i, tail t)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert mess Leaf =  Node Leaf mess Leaf 
insert newMess (Node left m right) =  
        if getTimeStamp newMess < getTimeStamp m 
        then Node (insert newMess left) m right
        else Node left m (insert newMess right)

getTimeStamp :: LogMessage -> Int 
getTimeStamp (LogMessage _ ts _ ) = ts 
getTimeStamp _ = error "OOPS" 

getText :: LogMessage -> String 
getText (LogMessage _ _ text ) = text 
getText (Unknown text ) = text 

build  :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ (m:inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getText (inOrder (build (filter (isBadError 10) msgs)))
 
isBadError :: Int -> LogMessage -> Bool 
isBadError lvl (LogMessage (Error sev) _ _) = sev >= lvl
isBadError _ _ = False 

