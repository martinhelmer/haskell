
import qualified Data.Map as Map
import Data.Functor

type UserName = String
type GamerId = Int
type PlayerCredits = Int
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThotf",150000)]


creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId gid = lookupUserName gid >>= lookupCredits

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB


hello name = "Hello, " ++ name ++ "!"

askForName :: IO()
askForName =  (putStrLn "What is your name?" >> getLine) Data.Functor.<&> hello >>= putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM  f v = v >>= (return . f)

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf v = mf >>= (`allFmapM` v)

maxPairM  :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM v = v >>= return . maximum