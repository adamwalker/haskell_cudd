{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module CuddInternal (
    CDdManager(..), 
    DdManager(..), 
    STDdManager(..), 
    CDdNode(..), 
    DdNode(..), 
    STDdNode(..), 
    cuddRef, 
    ddNodeToInt,
    deref,
    cudd_unique_slots,
    cudd_cache_slots
    ) where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Control.Monad.ST.Lazy
import Control.Monad
import Control.DeepSeq

import CuddC

#include <stdio.h>
#include "cudd.h"
#include "cuddwrap.h"

cudd_unique_slots :: Int
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

cudd_cache_slots :: Int
cudd_cache_slots = #const CUDD_CACHE_SLOTS

newtype DdManager = DdManager (Ptr CDdManager)

newtype STDdManager s u = STDdManager {unSTDdManager :: Ptr CDdManager}

newtype DdNode = DdNode {unDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

newtype STDdNode s u = STDdNode {unSTDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

instance NFData (STDdNode s u)

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode 

foreign import ccall safe "cudd.h &Cudd_RecursiveDeref"
	c_cuddRecursiveDerefPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_DelayedDerefBdd"
	c_cuddDelayedDerefBddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_IterDerefBdd"
	c_cuddIterDerefBddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

deref = c_cuddIterDerefBddPtr

