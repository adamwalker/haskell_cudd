{-#LANGUAGE ForeignFunctionInterface #-}

module Cudd.Reorder (
    CuddReorderingType(..),
    cuddReorderingStatus,
    cuddAutodynEnable,
    cuddAutodynDisable,
    cuddReduceHeap,
    cuddMakeTreeNode,
    cuddReadReorderingTime,
    cuddReadReorderings,
    cuddEnableReorderingReporting,
    cuddDisableReorderingReporting,
    cuddReorderingReporting,
    regStdPreReordHook,
    regStdPostReordHook,
    cuddTurnOnCountDead,
    cuddTurnOffCountDead,
    cuddDeadAreCounted,
    cuddReadSiftMaxSwap,
    cuddSetSiftMaxSwap,
    cuddReadSiftMaxVar,
    cuddSetSiftMaxVar,
    cuddReadNextReordering,
    cuddSetNextReordering,
    cuddReadMaxGrowthAlternate,
    cuddSetMaxGrowthAlternate,
    cuddReadMaxGrowth,
    cuddReadReorderingCycle,
    cuddSetReorderingCycle,
    cuddSetPopulationSize,
    cuddReadNumberXovers,
    cuddSetNumberXovers,
    regReordGCHook
    ) where

import System.IO
import System.Mem
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Cudd.C
import Cudd.Hook
import Cudd.Imperative

#include <stdio.h>
#include <cudd.h>

readIntegral :: (Integral i, Num j) => (Ptr CDDManager -> IO i) -> DDManager s u -> ST s j
readIntegral f (DDManager m) = unsafeIOToST $ liftM fromIntegral $ f m

setIntegral :: (Integral i, Num j) => (Ptr CDDManager -> j -> IO ()) -> DDManager s u -> i -> ST s ()
setIntegral f (DDManager m) v = unsafeIOToST $ f m (fromIntegral v)

readFloat :: (Real r, Fractional f) => (Ptr CDDManager -> IO r) -> DDManager s u -> ST s f
readFloat f (DDManager m) = unsafeIOToST $ liftM realToFrac $ f m 

setFloat :: (Real r, Fractional f) => (Ptr CDDManager -> f -> IO ()) -> DDManager s u -> r -> ST s ()
setFloat f (DDManager m) v = unsafeIOToST $ f m (realToFrac v)

--Reordering types
{#enum Cudd_ReorderingType as CuddReorderingType {underscoreToCase} deriving (Show, Eq) #}

--Reorder when needed
foreign import ccall safe "Cudd_ReorderingStatus"
	c_cuddReorderingStatus :: Ptr CDDManager -> Ptr CInt -> IO (CInt)

cuddReorderingStatus :: DDManager s u -> ST s (Int, CuddReorderingType)
cuddReorderingStatus (DDManager m) = unsafeIOToST $ do
	alloca $ \mem -> do
		res <- c_cuddReorderingStatus m mem
		typ <- peek mem
		return $ (fromIntegral res, toEnum $ fromIntegral typ)

foreign import ccall safe "Cudd_AutodynEnable"
	c_cuddAutodynEnable :: Ptr CDDManager -> CInt -> IO ()

cuddAutodynEnable :: DDManager s u -> CuddReorderingType -> ST s ()
cuddAutodynEnable (DDManager m) t = unsafeIOToST $ c_cuddAutodynEnable m (fromIntegral $ fromEnum t)

foreign import ccall safe "Cudd_AutodynDisable"
	c_cuddAutodynDisable :: Ptr CDDManager -> IO ()

cuddAutodynDisable :: DDManager s u -> ST s ()
cuddAutodynDisable (DDManager m) = unsafeIOToST $ c_cuddAutodynDisable m

--Reorder right now
foreign import ccall safe "Cudd_ReduceHeap"
	c_cuddReduceHeap :: Ptr CDDManager -> CInt -> CInt -> IO (CInt)

cuddReduceHeap :: DDManager s u -> CuddReorderingType -> Int -> ST s Int
cuddReduceHeap (DDManager m) typ minsize = unsafeIOToST $ liftM fromIntegral $ c_cuddReduceHeap m (fromIntegral $ fromEnum typ) (fromIntegral minsize)

--Grouping
foreign import ccall safe "Cudd_MakeTreeNode"
	c_cuddMakeTreeNode :: Ptr CDDManager -> CUInt -> CUInt -> CUInt -> IO (Ptr ())

cuddMakeTreeNode :: DDManager s u -> Int -> Int -> Int -> ST s (Ptr ())
cuddMakeTreeNode (DDManager m) low size typ = unsafeIOToST $ do
    res <- c_cuddMakeTreeNode m (fromIntegral low) (fromIntegral size) (fromIntegral typ)
    when (res==nullPtr) (error "cuddMakeTreeNode returned error")
    return res

--Reordering stats
foreign import ccall safe "Cudd_ReadReorderingTime"
	c_cuddReadReorderingTime :: Ptr CDDManager -> IO (CLong)

cuddReadReorderingTime :: DDManager s u -> ST s Int
cuddReadReorderingTime = readIntegral c_cuddReadReorderingTime

foreign import ccall safe "Cudd_ReadReorderings"
	c_cuddReadReorderings :: Ptr CDDManager -> IO (CUInt)

cuddReadReorderings :: DDManager s u -> ST s Int
cuddReadReorderings = readIntegral c_cuddReadReorderings

--Hooks
foreign import ccall safe "&Cudd_StdPreReordHook"
	c_cuddStdPreReordHook :: HookFP

foreign import ccall safe "&Cudd_StdPostReordHook"
	c_cuddStdPostReordHook :: HookFP

foreign import ccall safe "Cudd_EnableReorderingReporting"
	c_cuddEnableReorderingReporting :: Ptr CDDManager -> IO (CInt)

cuddEnableReorderingReporting :: DDManager s u -> ST s Int
cuddEnableReorderingReporting (DDManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddEnableReorderingReporting m

foreign import ccall safe "Cudd_DisableReorderingReporting"
	c_cuddDisableReorderingReporting :: Ptr CDDManager -> IO (CInt)

cuddDisableReorderingReporting :: DDManager s u -> ST s Int
cuddDisableReorderingReporting (DDManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddDisableReorderingReporting m

foreign import ccall safe "Cudd_ReorderingReporting"
	c_cuddReorderingReporting :: Ptr CDDManager -> IO (CInt)

cuddReorderingReporting :: DDManager s u -> ST s Int
cuddReorderingReporting (DDManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddReorderingReporting m

regStdPreReordHook :: DDManager s u -> ST s Int
regStdPreReordHook m = cuddAddHook m c_cuddStdPreReordHook CuddPreReorderingHook

regStdPostReordHook :: DDManager s u -> ST s Int
regStdPostReordHook m = cuddAddHook m c_cuddStdPostReordHook CuddPostReorderingHook

--Universal reordering params
foreign import ccall safe "Cudd_TurnOffCountDead"
	c_cuddTurnOffCountDead :: Ptr CDDManager -> IO ()

cuddTurnOffCountDead :: DDManager s u -> ST s ()
cuddTurnOffCountDead (DDManager m) = unsafeIOToST $ c_cuddTurnOffCountDead m

foreign import ccall safe "Cudd_TurnOnCountDead"
	c_cuddTurnOnCountDead :: Ptr CDDManager -> IO ()

cuddTurnOnCountDead :: DDManager s u -> ST s ()
cuddTurnOnCountDead (DDManager m) = unsafeIOToST $ c_cuddTurnOnCountDead m

foreign import ccall safe "Cudd_DeadAreCounted"
	c_cuddDeadAreCounted :: Ptr CDDManager -> IO CInt

cuddDeadAreCounted :: DDManager s u -> ST s Int
cuddDeadAreCounted (DDManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddDeadAreCounted m

--Sifting parameters
foreign import ccall safe "Cudd_ReadSiftMaxSwap"
	c_cuddReadSiftMaxSwap :: Ptr CDDManager -> IO (CInt)

cuddReadSiftMaxSwap :: DDManager s u -> ST s Int
cuddReadSiftMaxSwap = readIntegral c_cuddReadSiftMaxSwap

foreign import ccall safe "Cudd_SetSiftMaxSwap"
	c_cuddSetSiftMaxSwap :: Ptr CDDManager -> CInt -> IO ()

cuddSetSiftMaxSwap :: DDManager s u -> Int -> ST s ()
cuddSetSiftMaxSwap = setIntegral c_cuddSetSiftMaxSwap 

foreign import ccall safe "Cudd_ReadSiftMaxVar"
	c_cuddReadSiftMaxVar :: Ptr CDDManager -> IO (Int)

cuddReadSiftMaxVar :: DDManager s u -> ST s Int
cuddReadSiftMaxVar = readIntegral c_cuddReadSiftMaxVar

foreign import ccall safe "Cudd_SetSiftMaxVar"
	c_cuddSetSiftMaxVar :: Ptr CDDManager -> CInt -> IO ()

cuddSetSiftMaxVar :: DDManager s u -> Int -> ST s ()
cuddSetSiftMaxVar = setIntegral c_cuddSetSiftMaxVar
	
foreign import ccall safe "Cudd_ReadNextReordering"
	c_cuddReadNextReordering :: Ptr CDDManager -> IO (Int)

cuddReadNextReordering :: DDManager s u -> ST s Int
cuddReadNextReordering = readIntegral c_cuddReadNextReordering

foreign import ccall safe "Cudd_SetNextReordering"
	c_cuddSetNextReordering :: Ptr CDDManager -> CInt -> IO ()

cuddSetNextReordering :: DDManager s u -> Int -> ST s ()
cuddSetNextReordering = setIntegral c_cuddSetNextReordering

foreign import ccall safe "Cudd_ReadMaxGrowthAlternate"
	c_cuddReadMaxGrowthAlternate :: Ptr CDDManager -> IO CDouble

cuddReadMaxGrowthAlternate :: DDManager s u -> ST s Double
cuddReadMaxGrowthAlternate = readFloat c_cuddReadMaxGrowthAlternate

foreign import ccall safe "Cudd_SetMaxGrowthAlternate"
	c_cuddSetMaxGrowthAlternate :: Ptr CDDManager -> CDouble -> IO ()

cuddSetMaxGrowthAlternate :: DDManager s u -> Double -> ST s ()
cuddSetMaxGrowthAlternate = setFloat c_cuddSetMaxGrowthAlternate

foreign import ccall safe "Cudd_ReadMaxGrowth"
	c_cuddReadMaxGrowth :: Ptr CDDManager -> IO CDouble

cuddReadMaxGrowth :: DDManager s u -> ST s Double
cuddReadMaxGrowth = readFloat c_cuddReadMaxGrowth

foreign import ccall safe "Cudd_SetMaxGrowth"
	c_cuddSetMaxGrowth :: Ptr CDDManager -> CDouble -> IO ()

cuddSetMaxGrowth :: DDManager s u -> Double -> ST s ()
cuddSetMaxGrowth = setFloat c_cuddSetMaxGrowth

foreign import ccall safe "Cudd_ReadReorderingCycle"
	c_cuddReadReorderingCycle :: Ptr CDDManager -> IO (CInt)

cuddReadReorderingCycle :: DDManager s u -> ST s Int
cuddReadReorderingCycle = readIntegral c_cuddReadReorderingCycle

foreign import ccall safe "Cudd_SetReorderingCycle"
	c_cuddSetReorderingCycle :: Ptr CDDManager -> CInt -> IO ()

cuddSetReorderingCycle :: DDManager s u -> Int -> ST s ()
cuddSetReorderingCycle = setIntegral c_cuddSetReorderingCycle

--Genetic algorithm
foreign import ccall safe "Cudd_ReadPopulationSize"
	c_cuddReadPopulationSize :: Ptr CDDManager -> IO (CInt)

cuddReadPopulationSize :: DDManager s u -> ST s Int
cuddReadPopulationSize = readIntegral c_cuddReadPopulationSize

foreign import ccall safe "Cudd_SetPopulationSize"
	c_cuddSetPopulationSize :: Ptr CDDManager -> CInt -> IO ()

cuddSetPopulationSize :: DDManager s u -> Int -> ST s ()
cuddSetPopulationSize = setIntegral c_cuddSetPopulationSize

foreign import ccall safe "Cudd_ReadNumberXovers"
	c_cuddReadNumberXovers :: Ptr CDDManager -> IO (CInt)

cuddReadNumberXovers :: DDManager s u -> ST s Int
cuddReadNumberXovers = readIntegral c_cuddReadNumberXovers

foreign import ccall safe "Cudd_SetNumberXovers"
	c_cuddSetNumberXovers :: Ptr CDDManager -> CInt -> IO ()

cuddSetNumberXovers :: DDManager s u -> Int -> ST s ()
cuddSetNumberXovers = setIntegral c_cuddSetNumberXovers

reordGCHook :: HookTyp
reordGCHook _ _ _ = do
    performGC
    return 1

foreign import ccall "wrapper"
    makeFunPtr :: HookTyp -> IO (FunPtr HookTyp)

regReordGCHook :: DDManager s u -> ST s Int
regReordGCHook m = do
    hk <- unsafeIOToST $ makeFunPtr reordGCHook
    cuddAddHook m hk CuddPreReorderingHook

