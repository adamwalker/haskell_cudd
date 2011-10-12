{-#LANGUAGE ForeignFunctionInterface #-}

module CuddGC where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Control.Monad

import CuddInternal

#include <stdio.h>
#include <cudd.h>

foreign import ccall unsafe "cudd.h Cudd_EnableGarbageCollection"
	c_cuddEnableGarbageCollection :: Ptr CDdManager -> IO ()

cuddEnableGarbageCollection :: DdManager -> IO ()
cuddEnableGarbageCollection (DdManager m) = c_cuddEnableGarbageCollection m

foreign import ccall unsafe "cudd.h Cudd_DisableGarbageCollection"
	c_cuddDisableGarbageCollection :: Ptr CDdManager -> IO ()

cuddDisableGarbageCollection :: DdManager -> IO ()
cuddDisableGarbageCollection (DdManager m) = c_cuddDisableGarbageCollection m

foreign import ccall unsafe "cudd.h Cudd_GarbageCollectionEnabled"
	c_cuddGarbageCollectionEnabled :: Ptr CDdManager -> IO (CInt)

cuddGarbageCollectionEnabled :: DdManager -> IO (Int)
cuddGarbageCollectionEnabled (DdManager m) = liftM fromIntegral $ c_cuddGarbageCollectionEnabled m


