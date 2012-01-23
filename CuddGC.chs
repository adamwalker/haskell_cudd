{-#LANGUAGE ForeignFunctionInterface #-}

module CuddGC (
    cuddEnableGarbageCollection,
    cuddDisableGarbageCollection,
    cuddGarbageCollectionEnabled,
    regPreGCHook,
    regPostGCHook
    ) where

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
import CuddHook

#include <stdio.h>
#include <cudd.h>

foreign import ccall safe "cudd.h Cudd_EnableGarbageCollection"
	c_cuddEnableGarbageCollection :: Ptr CDdManager -> IO ()

cuddEnableGarbageCollection :: DdManager -> IO ()
cuddEnableGarbageCollection (DdManager m) = c_cuddEnableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_DisableGarbageCollection"
	c_cuddDisableGarbageCollection :: Ptr CDdManager -> IO ()

cuddDisableGarbageCollection :: DdManager -> IO ()
cuddDisableGarbageCollection (DdManager m) = c_cuddDisableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_GarbageCollectionEnabled"
	c_cuddGarbageCollectionEnabled :: Ptr CDdManager -> IO (CInt)

cuddGarbageCollectionEnabled :: DdManager -> IO (Int)
cuddGarbageCollectionEnabled (DdManager m) = liftM fromIntegral $ c_cuddGarbageCollectionEnabled m

foreign import ccall safe "cuddwrap.h &PreGCHook"
	c_PreGCHook :: HookFP

foreign import ccall safe "cuddwrap.h &PostGCHook"
	c_PostGCHook :: HookFP

regPreGCHook :: DdManager -> IO (Int)
regPreGCHook m = cuddAddHook m c_PreGCHook CuddPreGcHook

regPostGCHook :: DdManager -> IO (Int)
regPostGCHook m = cuddAddHook m c_PostGCHook CuddPostGcHook

