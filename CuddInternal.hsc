{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module CuddInternal (
    DdManager(..), 
    STDdManager(..), 
    DdNode(..), 
    DDNode(..),
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
import Control.Monad.ST
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

newtype DDNode s u = DDNode {unDDNode :: Ptr CDdNode} deriving (Ord, Eq, Show)

deref = c_cuddIterDerefBddPtr

