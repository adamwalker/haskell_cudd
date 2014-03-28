module CuddConvert where

import System.IO.Unsafe
import Foreign.ForeignPtr
import Control.Monad.ST

import CuddInternal
import CuddC

toDdNode :: DdManager -> DDNode s u -> DdNode
toDdNode (DdManager m) (DDNode d) = DdNode $ unsafePerformIO $ do cuddRef d
                                                                  newForeignPtrEnv deref m d

toDdManager :: STDdManager s u -> DdManager
toDdManager = DdManager . unSTDdManager

toDDNode :: DdNode -> ST s (DDNode s u)
toDDNode (DdNode fp) = unsafeIOToST $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ DDNode p

