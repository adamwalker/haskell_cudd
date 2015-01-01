{-#LANGUAGE ForeignFunctionInterface #-}

module Cudd.GC (
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
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Cudd.Hook
import Cudd.C
import Cudd.Imperative

foreign import ccall safe "Cudd_EnableGarbageCollection"
	c_cuddEnableGarbageCollection :: Ptr CDDManager -> IO ()

cuddEnableGarbageCollection :: DDManager s u -> ST s ()
cuddEnableGarbageCollection (DDManager m) = unsafeIOToST $ c_cuddEnableGarbageCollection m

foreign import ccall safe "Cudd_DisableGarbageCollection"
	c_cuddDisableGarbageCollection :: Ptr CDDManager -> IO ()

cuddDisableGarbageCollection :: DDManager s u -> ST s ()
cuddDisableGarbageCollection (DDManager m) = unsafeIOToST $ c_cuddDisableGarbageCollection m

foreign import ccall safe "Cudd_GarbageCollectionEnabled"
	c_cuddGarbageCollectionEnabled :: Ptr CDDManager -> IO CInt

cuddGarbageCollectionEnabled :: DDManager s u -> ST s Int
cuddGarbageCollectionEnabled (DDManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddGarbageCollectionEnabled m

foreign import ccall safe "&preGCHook_sample"
	c_preGCHook_sample :: HookFP

foreign import ccall safe "&postGCHook_sample"
	c_postGCHook_sample :: HookFP

regPreGCHook :: DDManager s u -> HookFP -> ST s Int
regPreGCHook m func = cuddAddHook m func CuddPreGcHook

regPostGCHook :: DDManager s u -> HookFP -> ST s Int
regPostGCHook m func = cuddAddHook m func CuddPostGcHook

