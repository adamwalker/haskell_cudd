module CuddConvert where

import System.IO.Unsafe
import Foreign.ForeignPtr

import CuddInternal
import CuddExplicitDeref hiding (deref)

toDdNode :: DdManager -> DDNode s u -> DdNode
toDdNode (DdManager m) (DDNode d) = DdNode $ unsafePerformIO $ newForeignPtrEnv deref m d

toDdManager :: STDdManager s u -> DdManager
toDdManager = DdManager . unSTDdManager
