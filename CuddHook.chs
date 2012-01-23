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

import CuddInternal

#include <stdio.h>
#include <cudd.h>

{#enum Cudd_HookType as CuddHookType {underscoreToCase} deriving (Show, Eq) #}

type HookTyp = Ptr CDdManager -> CString -> Ptr () -> IO (CInt)
type HookFP  = FunPtr HookTyp

foreign import ccall safe "cudd.h Cudd_AddHook"
	c_cuddAddHook :: Ptr CDdManager -> HookFP -> CInt -> IO (CInt)

cuddAddHook :: DdManager -> HookFP -> CuddHookType -> IO Int
cuddAddHook (DdManager m) fp typ = liftM fromIntegral $ c_cuddAddHook m fp (fromIntegral $ fromEnum typ)
	
foreign import ccall safe "cudd.h Cudd_RemoveHook"
	c_cuddRemoveHook :: Ptr CDdManager -> HookFP -> CInt -> IO (CInt)

cuddRemoveHook :: DdManager -> HookFP -> CuddHookType -> IO Int
cuddRemoveHook (DdManager m) fp typ = liftM fromIntegral $ c_cuddRemoveHook m fp (fromIntegral $ fromEnum typ)
