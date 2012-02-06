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
import Control.Monad.ST.Lazy

import CuddInternal
import CuddHook

#include <stdio.h>
#include <cudd.h>

foreign import ccall safe "cudd.h Cudd_EnableGarbageCollection"
	c_cuddEnableGarbageCollection :: Ptr CDdManager -> IO ()

cuddEnableGarbageCollection :: STDdManager s -> ST s ()
cuddEnableGarbageCollection (STDdManager m) = unsafeIOToST $ c_cuddEnableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_DisableGarbageCollection"
	c_cuddDisableGarbageCollection :: Ptr CDdManager -> IO ()

cuddDisableGarbageCollection :: STDdManager s -> ST s ()
cuddDisableGarbageCollection (STDdManager m) = unsafeIOToST $ c_cuddDisableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_GarbageCollectionEnabled"
	c_cuddGarbageCollectionEnabled :: Ptr CDdManager -> IO (CInt)

cuddGarbageCollectionEnabled :: STDdManager s -> ST s Int
cuddGarbageCollectionEnabled (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddGarbageCollectionEnabled m

foreign import ccall safe "cuddwrap.h &PreGCHook"
	c_PreGCHook :: HookFP

foreign import ccall safe "cuddwrap.h &PostGCHook"
	c_PostGCHook :: HookFP

regPreGCHook :: STDdManager s -> ST s Int
regPreGCHook m = cuddAddHook m c_PreGCHook CuddPreGcHook

regPostGCHook :: STDdManager s -> ST s Int
regPostGCHook m = cuddAddHook m c_PostGCHook CuddPostGcHook

