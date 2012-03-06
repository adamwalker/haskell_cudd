{-# LANGUAGE ForeignFunctionInterface, CPP, RankNTypes #-}

module CuddST (
        cuddInitST,
        withManagerST,
        withManagerIO,
        cuddShuffleHeapST,
        cuddSetVarMapST,
        cuddBddVarMapST,
        cuddBddIthVarST
    ) where

import System.IO
import System.Directory
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Control.Monad.ST.Lazy
import Control.Monad
import Data.Binary
import Data.List
import Control.DeepSeq
import Control.Monad.Error
import Data.Array hiding (indices)
import Control.Exception hiding (catch)

import CuddInternal

#include "cudd.h"

deref = c_cuddIterDerefBdd

cudd_unique_slots :: Int
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

cudd_cache_slots :: Int
cudd_cache_slots = #const CUDD_CACHE_SLOTS

foreign import ccall safe "cudd.h Cudd_Init"
	c_cuddInit :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CDdManager)

cuddInitST :: ST s (STDdManager s u)
cuddInitST = unsafeIOToST $ do
    cm <- c_cuddInit 0 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0
    return $ STDdManager cm

withManagerST :: (forall u. STDdManager s u -> ST s a) -> ST s a
withManagerST f = do
    res <- cuddInitST
    f res 

withManagerIO :: MonadIO m => (forall u. STDdManager RealWorld u -> m a) -> m a
withManagerIO f = do
    res <- liftIO $ stToIO cuddInitST
    f res

foreign import ccall safe "cudd.h Cudd_ShuffleHeap"
    c_cuddShuffleHeap :: Ptr CDdManager -> Ptr CInt -> IO CInt

cuddShuffleHeapST :: STDdManager s u -> [Int] -> ST s ()
cuddShuffleHeapST (STDdManager m) order = unsafeIOToST $ 
    withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "cuddInitOrder: order does not contain each variable once") 
    res1 <- c_cuddBddIthVar m (fromIntegral (size - 1))
    when (res1 == nullPtr) (error "cuddShuffleHeapST: Failed to resize table")
    res2 <- c_cuddShuffleHeap m ptr
    when (fromIntegral res2 /= 1) (error "cuddShuffleHeapST: Cudd_ShuffleHeap failed")
    return ()

foreign import ccall safe "cudd.h Cudd_SetVarMap"
    c_cuddSetVarMap :: Ptr CDdManager -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> CInt -> IO CInt

cuddSetVarMapST :: STDdManager s u -> [STDdNode s u] -> [STDdNode s u] -> ST s ()
cuddSetVarMapST (STDdManager m) v1 v2 = unsafeIOToST $ 
    withForeignArrayPtrLen (map unSTDdNode v1) $ \s1 v1p -> 
    withForeignArrayPtrLen (map unSTDdNode v2) $ \s2 v2p -> do
    when (s1 /= s2) (error "cuddSetVarMapST: variable list lengths are not equal")
    res <- c_cuddSetVarMap m v1p v2p (fromIntegral s1)
    when (fromIntegral res /= 1) (error "cuddSetVarMapST: Cudd_SetVarMap failed")
    return ()

foreign import ccall safe "cudd.h Cudd_bddVarMap_s"
    c_cuddBddVarMap :: Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)

cuddBddVarMapST :: STDdManager s u -> STDdNode s u -> ST s (STDdNode s u)
cuddBddVarMapST (STDdManager m) (STDdNode node) = unsafeIOToST $ 
    withForeignPtr node $ \np -> do
    node <- c_cuddBddVarMap m np
    fp <- newForeignPtrEnv deref m node
    return $ STDdNode fp

foreign import ccall safe "cudd.h Cudd_bddIthVar_s"
	c_cuddBddIthVar :: Ptr CDdManager -> CInt -> IO (Ptr CDdNode)

cuddBddIthVarST :: STDdManager s u -> Int -> ST s (STDdNode s u)
cuddBddIthVarST (STDdManager d) i = (liftM STDdNode) $ unsafeIOToST $ do
    node <- c_cuddBddIthVar d (fromIntegral i)
    cuddRef node
    newForeignPtr_ node
