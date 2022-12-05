module Day13 where
import           AOCHelper
import           IntCode
import           Data.Maybe
import           Data.List.Split
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as M
import           Control.Monad
import           Control.Concurrent

f :: IO Int
f = do
    a <- getChar
    return $ case a of
                'a' -> -1
                's' -> 0
                'd' -> 1

ff pp bp | pp < bp = 1
         | pp == bp = 0
         | pp > bp = -1

run :: IO ()
run = do
   putStrLn "--- Day 13: Care Package ---"
   comp <-  readInp "input13.txt" >>= \s ->return $ initComp (parseString s:: UV.Vector Int ) []
   part1 comp
   readInp "input13.txt" >>= part2

part1 :: PComputer  -> IO PComputer
part1 c = do
    let out = output $ runComp c
        ops = filter (==2) $ map snd $ filter fst $ zip (concat $ repeat [True,False,False]) out
    putStr " Part 1: "
    print $ length ops
    return $ runComp c

part2 :: String  -> IO ()
part2 s = do
    putStr " Part 2: "
    let v = (parseString s :: UV.Vector Int) UV.// [(0,2)]
        c =  runComp $ initComp v []
        screen = (M.empty,0)
        positions = getPositions c (0,0)
    let loop c screen positions
            | state c == Halted = return (c, getDisplay c screen)
            | otherwise = do
            let (paddlepos, ballpos) = getPositions c positions
                oldScore = snd screen
                newScreen = getDisplay c screen
            -- when (oldScore /= snd newScreen) $ do
            --      putStrLn $ "   " ++ show paddlepos ++ "   " ++ show ballpos
            --      printScreen newScreen
            let c2 = runComp $ clearOut . addInp c $ ff paddlepos ballpos
            loop c2 newScreen (paddlepos, ballpos)
    (c2,d) <- loop c screen positions
    printScreen d


getPositions :: Computer v -> (Int, Int) -> (Int, Int)
getPositions c (a,b)= (fromMaybe a $ listToMaybe ppl, fromMaybe b $ listToMaybe bpl)
    where  ppl = map (fst . fst)  $ filter (\((x,y),v) -> v == 3)  $ output2list c
           bpl = map (fst . fst)  $ filter (\((x,y),v) -> v == 4)  $ output2list c

getDisplay :: Computer v -> (M.Map (Int, Int) Int, Int) -> (M.Map (Int, Int) Int, Int)
getDisplay c (m,s) = (M.union  (M.delete (-1,0) m') m, fromMaybe s $ M.lookup (-1,0) m')
    where m' = M.fromList $ output2list c

output2list c = go [] (reverse $ output c)
    where go o [] = o
          go o (a:b:c:xs) = go (((a,b),c):o) xs

printScreen (m,s) = do
        print s
        putStrLn $ draw2dmap m
--
