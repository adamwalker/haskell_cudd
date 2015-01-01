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

import Cudd.Cudd as C
import Cudd.C
import Cudd.Imperative as I hiding (deref)

fromImperativeNode :: C.DDManager -> I.DDNode s u -> C.DDNode
fromImperativeNode (C.DDManager m) (I.DDNode d) = C.DDNode $ unsafePerformIO $ do 
    cuddRef d
    newForeignPtrEnv deref m d

fromImperativeManager :: I.DDManager s u -> C.DDManager
fromImperativeManager = C.DDManager . I.unDDManager

toImperativeNode :: C.DDNode -> ST s (I.DDNode s u)
toImperativeNode (C.DDNode fp) = unsafeIOToST $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ I.DDNode p

toImperativeManager :: C.DDManager -> I.DDManager s u
toImperativeManager (C.DDManager m) = I.DDManager m
