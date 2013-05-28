{-# LANGUAGE ForeignFunctionInterface, CPP, RankNTypes #-}

module CuddST (
        cuddInitST,
        cuddInitSTDefaults,
        withManagerST,
        withManagerSTDefaults,
        withManagerIO,
        withManagerIODefaults,
        cuddShuffleHeapST,
        cuddSetVarMapST,
        cuddBddVarMapST,
        cuddBddIthVarST
    ) where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Control.Monad.ST
import Control.Monad
import Data.List
import Control.Monad.IO.Class

import CuddC
import CuddInternal
import ForeignHelpers

#include <stdio.h>
#include "cudd.h"

cuddInitST :: Int -> Int -> Int -> Int -> Int -> ST s (STDdManager s u)
cuddInitST numVars numVarsZ numSlots cacheSize maxMemory = unsafeIOToST $ do
    cm <- c_cuddInit (fromIntegral numVars) (fromIntegral numVarsZ) (fromIntegral numSlots) (fromIntegral cacheSize) (fromIntegral maxMemory)
    return $ STDdManager cm

cuddInitSTDefaults :: ST s (STDdManager s u)
cuddInitSTDefaults = cuddInitST 0 0 cudd_unique_slots cudd_cache_slots 0

withManagerST :: Int -> Int -> Int -> Int -> Int -> (forall u. STDdManager s u -> ST s a) -> ST s a
withManagerST numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- cuddInitST numVars numVarsZ numSlots cacheSize maxMemory
    f res 

withManagerSTDefaults :: (forall u. STDdManager s u -> ST s a) -> ST s a
withManagerSTDefaults f = do
    res <- cuddInitSTDefaults
    f res 

withManagerIO :: MonadIO m => Int -> Int -> Int -> Int -> Int -> (forall u. STDdManager RealWorld u -> m a) -> m a
withManagerIO numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- liftIO $ stToIO $ cuddInitST numVars numVarsZ numSlots cacheSize maxMemory
    f res

withManagerIODefaults :: MonadIO m => (forall u. STDdManager RealWorld u -> m a) -> m a
withManagerIODefaults f = do
    res <- liftIO $ stToIO cuddInitSTDefaults
    f res

cuddShuffleHeapST :: STDdManager s u -> [Int] -> ST s ()
cuddShuffleHeapST (STDdManager m) order = unsafeIOToST $ 
    withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "cuddShuffleHeadST: order does not contain each variable once") 
    res1 <- c_cuddBddIthVar m (fromIntegral (size - 1))
    when (res1 == nullPtr) (error "cuddShuffleHeapST: Failed to resize table")
    res2 <- c_cuddShuffleHeap m ptr
    when (fromIntegral res2 /= 1) (error "cuddShuffleHeapST: Cudd_ShuffleHeap failed")
    return ()

cuddSetVarMapST :: STDdManager s u -> [STDdNode s u] -> [STDdNode s u] -> ST s ()
cuddSetVarMapST (STDdManager m) v1 v2 = unsafeIOToST $ 
    withForeignArrayPtrLen (map unSTDdNode v1) $ \s1 v1p -> 
    withForeignArrayPtrLen (map unSTDdNode v2) $ \s2 v2p -> do
    when (s1 /= s2) (error "cuddSetVarMapST: variable list lengths are not equal")
    res <- c_cuddSetVarMap m v1p v2p (fromIntegral s1)
    when (fromIntegral res /= 1) (error "cuddSetVarMapST: Cudd_SetVarMap failed")
    return ()

cuddBddVarMapST :: STDdManager s u -> STDdNode s u -> ST s (STDdNode s u)
cuddBddVarMapST (STDdManager m) (STDdNode node) = unsafeIOToST $ 
    withForeignPtr node $ \np -> do
    node <- c_cuddBddVarMap m np
    fp <- newForeignPtrEnv deref m node
    return $ STDdNode fp

cuddBddIthVarST :: STDdManager s u -> Int -> ST s (STDdNode s u)
cuddBddIthVarST (STDdManager d) i = (liftM STDdNode) $ unsafeIOToST $ do
    node <- c_cuddBddIthVar d (fromIntegral i)
    cuddRef node
    newForeignPtr_ node

