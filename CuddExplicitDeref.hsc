module CuddExplicitDeref (
    bzero,
    bone,
    bvar,
    band,
    bor,
    bnand,
    bnor,
    bxor,
    bxnor,
    bnot,
    bite,
    bexists,
    bforall,
    deref,
    setVarMap,
    mapVars
    ) where

import Foreign hiding (void)
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.ST.Lazy
import Control.Monad

import CuddC
import CuddInternal hiding (deref)

newtype DDNode s u = DDNode {unDDNode :: Ptr CDdNode} deriving (Ord, Eq, Show)

arg0 :: (Ptr CDdManager -> IO (Ptr CDdNode)) -> STDdManager s u -> ST s (DDNode s u)
arg0 f (STDdManager m) = liftM DDNode $ unsafeIOToST $ f m

arg1 :: (Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> ST s (DDNode s u)
arg1 f (STDdManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ f m x

arg2 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> DDNode s u -> ST s (DDNode s u)
arg2 f (STDdManager m) (DDNode x) (DDNode y) = liftM DDNode $ unsafeIOToST $ f m x y
    
arg3 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> DDNode s u -> DDNode s u  -> ST s (DDNode s u)
arg3 f (STDdManager m) (DDNode x) (DDNode y) (DDNode z) = liftM DDNode $ unsafeIOToST $ f m x y z

bzero   = arg0 c_cuddReadLogicZero
bone    = arg0 c_cuddReadOne
bvar i  = arg0 (flip c_cuddBddIthVar (fromIntegral i))
band    = arg2 c_cuddBddAnd
bor     = arg2 c_cuddBddOr
bnand   = arg2 c_cuddBddNand
bnor    = arg2 c_cuddBddNor
bxor    = arg2 c_cuddBddXor
bxnor   = arg2 c_cuddBddXnor
bnot    = arg1 (const c_cuddNot)
bite    = arg3 c_cuddBddIte
bexists = arg2 c_cuddBddExistAbstract
bforall = arg2 c_cuddBddUnivAbstract

deref :: STDdManager s u -> DDNode s u -> ST s ()
deref (STDdManager m) (DDNode x) = unsafeIOToST $ c_cuddIterDerefBdd m x

setVarMap :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> ST s ()
setVarMap (STDdManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "setVarMap: lengths not equal")
    void $ c_cuddSetVarMap m xp yp (fromIntegral xl)

mapVars :: STDdManager s u -> DDNode s u -> ST s (DDNode s u)
mapVars (STDdManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ c_cuddBddVarMap m x

{-
refCount :: STDdManager s u -> STDdNode s u -> ST s (DdNode s u)

unRefCount :: STDdManager s u -> DdNode s u -> ST s (STDdNode s u)
-}
