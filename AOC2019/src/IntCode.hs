{-# LANGUAGE FlexibleInstances #-}
module IntCode where

import AOCHelper
import Data.Maybe (isNothing)
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import Data.Functor
import Control.Monad.Loops
import Debug.Trace
import Data.Char 

data CState = Running | Halted | WaitForInput deriving (Show, Eq)

data Computer v= Computer  { pos :: Int,
                             input :: [Int],
                             output :: [Int],
                             vector :: v,
                             state :: CState,
                             relbase :: Int
                           } deriving (Show, Eq)
type MComputer s = Computer (V.MVector s Int)
type PComputer = Computer (V.Vector Int)

showOut :: Computer v -> [Char]
showOut c =  init $ map chr . reverse .  output $ c

initComp :: v -> [Int] -> Computer v
initComp v inp = Computer 0 inp [] v Running 0


setInstr :: V.Unbox a => Computer (V.Vector a) -> Int -> a -> Computer (V.Vector a)
setInstr (Computer p inps o vec s r)  addr instr = Computer p inps o (vec V.// [(addr,instr)]) s r


addInp :: Computer v -> Int -> Computer v
addInp (Computer p inps o v s r) i = Computer p (inps++[i]) o v s r

setInp :: Computer v -> [Int] -> Computer v
setInp (Computer p _ o v s r) i = Computer p i o v s r

clearOut :: Computer v -> Computer v
clearOut (Computer p i _ v s r) = Computer p i [] v s r

modRel :: Int -> Computer v  -> Computer v
modRel x (Computer p inps o v s r) =  Computer p inps o v s (r+x)

setState ::  Computer v -> CState -> Computer v
setState (Computer p inps o v _ r) s =  Computer p inps o v s r

setV :: v1 -> Computer v2 -> Computer v1
setV v (Computer r inputs outputs _ s rel) = Computer r inputs outputs v s rel

step ::  Int -> Computer v -> Computer v
step x (Computer p inps o v s r) =  Computer (p+x) inps o v s r

runProgram :: (PrimMonad m) => PComputer -> [(Int,Int)] -> m PComputer
runProgram pc alter  = do
   v <- setup (vector pc) alter
   let c = Computer (pos pc) (input pc) (output pc) v Running (relbase pc)
   let loop c = if state c /= Running then return c else doInstr c >>= loop
   (Computer p i o v s r) <- loop c
   outV <- V.freeze v
   return $ Computer p i o outV s r

-- just thaw the vector and modify initial state based on "alter" 
setup :: (V.Unbox a, PrimMonad m, Foldable t) => V.Vector a -> t (Int, a) -> m (V.MVector (PrimState m) a)
setup v alter = do
     mv <-  V.thaw v
     forM_ alter $ \(i,v) -> do
        VM.write mv i v
     return mv

-- alternative formulation of setup 
setup' v l = V.thaw v >>= \mv -> foldr ((>>) . uncurry (VM.write mv)) (return ()) l >> return mv

doInstr :: PrimMonad m => MComputer (PrimState m) -> m (MComputer (PrimState m))
doInstr c@(Computer p inputs outputs v state rel) = do
   instruction <- safeRead v p
   let opCode = instruction `rem` 100
   -- let a = trace "" 0 
   case opCode of
      99 -> return $ setState c Halted
      1 ->  addOrMul (+) instruction
      2 ->  addOrMul (*) instruction
      3 ->  case inputs of                                           -- head:input -> dest
            [] -> return $ setState c WaitForInput
            inp -> do
               v2 <- writeTo v 1 (head inp) instruction
               return (Computer (p+2) (tail inputs) outputs v2 Running rel)
      4 -> do                                                        -- src-> output
            out <- readFrom v 1 instruction
            return (Computer (p+2) inputs (out:outputs) v Running rel)
      5 -> jumpIf v (>0) instruction
      6 -> jumpIf v (==0) instruction
      7 -> addOrMul (\l r -> if l < r then 1 else 0) instruction
      8 -> addOrMul (\l r -> if l == r then 1 else 0) instruction
      9 -> readFrom v 1 instruction >>= \x -> return . step 2 . modRel x $ c

      _ -> error $ ":"++ show instruction ++","++show p

   where addOrMul op instr = do
            arg1 <- readFrom v 1 instr
            arg2 <- readFrom v 2 instr
            v2 <- writeTo v 3 (op arg1 arg2) instr
            return . step 4 . setV v2 $ c

         mode instr n = instr `div` 10^(n+1) `rem` 10

         jumpIf :: PrimMonad m => V.MVector (PrimState m) Int -> (Int -> Bool) -> Int  -> m (MComputer (PrimState m))
         jumpIf v pred instr = do
               q <- readFrom v 1 instr
               r <- if pred q then readFrom v 2 instr else return (p+3)
               return (Computer r inputs outputs v Running rel)

         readFrom v n instr = safeRead v (p+n) >>= \a -> if mode instr n /= 1 then safeRead v (a + relmod) else return a
            where relmod = if mode instr n == 2 then rel else 0

         writeTo v n val instr = do
               ix <- safeRead v (p+n) >>= \x -> return $ if mode instr n == 2 then rel + x else x
               safeWrite v ix val

safeRead :: PrimMonad m => V.MVector (PrimState m) Int -> Int -> m Int
safeRead v ix = if ix >= VM.length v then return 0 else VM.read v ix

safeWrite :: PrimMonad m => V.MVector (PrimState m) Int -> Int -> Int -> m (V.MVector (PrimState m) Int )
safeWrite v ix val | ix < l =VM.write v ix val >> return v
                   | otherwise = do
                        v2 <- VM.grow v (1 +ix - l)
                        forM_ [l..(ix-1)] $ \i -> do
                              VM.write v2 i 0
                        VM.write v2 ix val
                        return v2
      where l = VM.length v



runProg :: V.Vector Int -> [Int] -> PComputer
runProg v i = runST $ runProgram (initComp v i) []

runComp :: PComputer -> PComputer
runComp c = runST $ runProgram c []

instance Parseable PComputer  where
    parseString s = initComp (V.fromList (read $ "[" ++ s ++ "]")) []

