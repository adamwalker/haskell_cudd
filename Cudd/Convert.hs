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
import Cudd.Cudd as C
import Cudd.C
import Cudd.Imperative as I hiding (deref)

fromImperativeNode :: DDManager -> I.DDNode s u -> C.DDNode
fromImperativeNode (DDManager m) (I.DDNode d) = C.DDNode $ unsafePerformIO $ do 
    cuddRef d
    newForeignPtrEnv deref m d

fromImperativeManager :: STDdManager s u -> DDManager
fromImperativeManager = DDManager . unSTDdManager

toImperativeNode :: C.DDNode -> ST s (I.DDNode s u)
toImperativeNode (C.DDNode fp) = unsafeIOToST $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ I.DDNode p

toImperativeManager :: DDManager -> STDdManager s u
toImperativeManager (DDManager m) = STDdManager m
