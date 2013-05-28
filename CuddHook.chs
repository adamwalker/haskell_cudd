{-#LANGUAGE ForeignFunctionInterface #-}

module CuddHook (
    cuddAddHook,
    cuddRemoveHook,
    HookTyp,
    HookFP,
    CuddHookType(..)
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

#include <stdio.h>
#include <cudd.h>

{#enum Cudd_HookType as CuddHookType {underscoreToCase} deriving (Show, Eq) #}

type HookTyp = Ptr CDdManager -> CString -> Ptr () -> IO (CInt)
type HookFP  = FunPtr HookTyp

foreign import ccall safe "cudd.h Cudd_AddHook"
	c_cuddAddHook :: Ptr CDdManager -> HookFP -> CInt -> IO (CInt)

cuddAddHook :: STDdManager s u -> HookFP -> CuddHookType -> ST s Int
cuddAddHook (STDdManager m) fp typ = unsafeIOToST $ liftM fromIntegral $ c_cuddAddHook m fp (fromIntegral $ fromEnum typ)
	
foreign import ccall safe "cudd.h Cudd_RemoveHook"
	c_cuddRemoveHook :: Ptr CDdManager -> HookFP -> CInt -> IO (CInt)

cuddRemoveHook :: STDdManager s u -> HookFP -> CuddHookType -> ST s Int
cuddRemoveHook (STDdManager m) fp typ = unsafeIOToST $ liftM fromIntegral $ c_cuddRemoveHook m fp (fromIntegral $ fromEnum typ)
