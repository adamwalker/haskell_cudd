module Cudd.Convert (
    fromImperativeNode,
    fromImperativeManager,
    toImperativeNode,
    toImperativeManager
    ) where

import System.IO.Unsafe
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Cudd.Internal
import Cudd.Cudd
import Cudd.C
import Cudd.Imperative hiding (deref)

fromImperativeNode :: DdManager -> DDNode s u -> DdNode
fromImperativeNode (DdManager m) (DDNode d) = DdNode $ unsafePerformIO $ do 
    cuddRef d
    newForeignPtrEnv deref m d

fromImperativeManager :: STDdManager s u -> DdManager
fromImperativeManager = DdManager . unSTDdManager

toImperativeNode :: DdNode -> ST s (DDNode s u)
toImperativeNode (DdNode fp) = unsafeIOToST $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ DDNode p

toImperativeManager :: DdManager -> STDdManager s u
toImperativeManager (DdManager m) = STDdManager m
