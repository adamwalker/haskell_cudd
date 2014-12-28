module Cudd.Convert (
    toDdNode,
    toDdManager,
    toDDNode,
    getSTManager
    ) where

import System.IO.Unsafe
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Cudd.Internal
import Cudd.C

toDdNode :: DdManager -> DDNode s u -> DdNode
toDdNode (DdManager m) (DDNode d) = DdNode $ unsafePerformIO $ do 
    cuddRef d
    newForeignPtrEnv deref m d

toDdManager :: STDdManager s u -> DdManager
toDdManager = DdManager . unSTDdManager

toDDNode :: DdNode -> ST s (DDNode s u)
toDDNode (DdNode fp) = unsafeIOToST $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ DDNode p

getSTManager :: DdManager -> STDdManager s u
getSTManager (DdManager m) = STDdManager m
