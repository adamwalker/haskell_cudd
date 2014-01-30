{-#LANGUAGE ForeignFunctionInterface #-}

module CuddGC (
    cuddEnableGarbageCollection,
    cuddDisableGarbageCollection,
    cuddGarbageCollectionEnabled,
    c_preGCHook_sample,
    c_postGCHook_sample,
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
import Control.Monad.ST

import CuddInternal
import CuddHook

foreign import ccall safe "cudd.h Cudd_EnableGarbageCollection"
	c_cuddEnableGarbageCollection :: Ptr CDdManager -> IO ()

cuddEnableGarbageCollection :: STDdManager s u -> ST s ()
cuddEnableGarbageCollection (STDdManager m) = unsafeIOToST $ c_cuddEnableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_DisableGarbageCollection"
	c_cuddDisableGarbageCollection :: Ptr CDdManager -> IO ()

cuddDisableGarbageCollection :: STDdManager s u -> ST s ()
cuddDisableGarbageCollection (STDdManager m) = unsafeIOToST $ c_cuddDisableGarbageCollection m

foreign import ccall safe "cudd.h Cudd_GarbageCollectionEnabled"
	c_cuddGarbageCollectionEnabled :: Ptr CDdManager -> IO (CInt)

cuddGarbageCollectionEnabled :: STDdManager s u -> ST s Int
cuddGarbageCollectionEnabled (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddGarbageCollectionEnabled m

foreign import ccall safe "cuddwrap.h &preGCHook_sample"
	c_preGCHook_sample :: HookFP

foreign import ccall safe "cuddwrap.h &postGCHook_sample"
	c_postGCHook_sample :: HookFP

regPreGCHook :: STDdManager s u -> HookFP -> ST s Int
regPreGCHook m func = cuddAddHook m func CuddPreGcHook

regPostGCHook :: STDdManager s u -> HookFP -> ST s Int
regPostGCHook m func = cuddAddHook m func CuddPostGcHook

