{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module Cudd.Internal (
    deref,
    cudd_unique_slots,
    cudd_cache_slots
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr

import Cudd.C

#include <stdio.h>
#include "cudd.h"

cudd_unique_slots :: Int
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

cudd_cache_slots :: Int
cudd_cache_slots = #const CUDD_CACHE_SLOTS

deref = c_cuddIterDerefBddPtr

