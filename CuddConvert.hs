module CuddConvert where

import System.IO.Unsafe
import Foreign.ForeignPtr

import CuddInternal
import CuddC

toDdNode :: DdManager -> DDNode s u -> DdNode
toDdNode (DdManager m) (DDNode d) = DdNode $ unsafePerformIO $ newForeignPtrEnv deref m d

toDdManager :: STDdManager s u -> DdManager
toDdManager = DdManager . unSTDdManager

toDDNode :: DdNode -> DDNode s u 
toDDNode (DdNode fp) = unsafePerformIO $ do
    let p = unsafeForeignPtrToPtr fp
    cuddRef p
    return $ DDNode p

