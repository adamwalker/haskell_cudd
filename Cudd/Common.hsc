module Cudd.Common (
    SatBit(..),
    toSatBit,
    expand,
    cudd_unique_slots,
    cudd_cache_slots
    ) where

#include <stdio.h>
#include "cudd.h"

data SatBit = Zero | One | DontCare deriving (Eq)

toSatBit :: Int -> SatBit
toSatBit 0 = Zero
toSatBit 1 = One
toSatBit 2 = DontCare
toSatBit _ = error "toSatBit: Invalid sat bit returned from CUDD"

expand :: SatBit -> [Bool]
expand Zero     = [False]
expand One      = [True]
expand DontCare = [False, True]

cudd_unique_slots :: Int
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

cudd_cache_slots :: Int
cudd_cache_slots = #const CUDD_CACHE_SLOTS

