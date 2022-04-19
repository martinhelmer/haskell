module T where 
  
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Array.Base as GM

-- main = do
--   v <- GM.new 10 :: IO (V.MVector RealWorld Int)
--   GM.write v 0 (3::Int)
--   putStrLn $ show $ GM.freeze v 
--   x <- GM.read v 0
--   putStrLn $ show x
