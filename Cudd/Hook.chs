{-#LANGUAGE ForeignFunctionInterface #-}

module Cudd.Hook (
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
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Cudd.C
import Cudd.Imperative

#include <stdio.h>
#include <cudd.h>

{#enum Cudd_HookType as CuddHookType {underscoreToCase} deriving (Show, Eq) #}

type HookTyp = Ptr CDDManager -> CString -> Ptr () -> IO (CInt)
type HookFP  = FunPtr HookTyp

foreign import ccall safe "Cudd_AddHook"
	c_cuddAddHook :: Ptr CDDManager -> HookFP -> CInt -> IO (CInt)

cuddAddHook :: DDManager s u -> HookFP -> CuddHookType -> ST s Int
cuddAddHook (DDManager m) fp typ = unsafeIOToST $ liftM fromIntegral $ c_cuddAddHook m fp (fromIntegral $ fromEnum typ)
	
foreign import ccall safe "Cudd_RemoveHook"
	c_cuddRemoveHook :: Ptr CDDManager -> HookFP -> CInt -> IO (CInt)

cuddRemoveHook :: DDManager s u -> HookFP -> CuddHookType -> ST s Int
cuddRemoveHook (DDManager m) fp typ = unsafeIOToST $ liftM fromIntegral $ c_cuddRemoveHook m fp (fromIntegral $ fromEnum typ)
