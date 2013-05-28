{-#LANGUAGE ForeignFunctionInterface #-}

module CuddReorder (
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
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Control.Monad
import Control.Monad.ST

import CuddInternal
import CuddHook

#include <stdio.h>
#include <cudd.h>

readIntegral :: (Integral i, Num j) => (Ptr CDdManager -> IO i) -> STDdManager s u -> ST s j
readIntegral f (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ f m

setIntegral :: (Integral i, Num j) => (Ptr CDdManager -> j -> IO ()) -> STDdManager s u -> i -> ST s ()
setIntegral f (STDdManager m) v = unsafeIOToST $ f m (fromIntegral v)

readFloat :: (Real r, Fractional f) => (Ptr CDdManager -> IO r) -> STDdManager s u -> ST s f
readFloat f (STDdManager m) = unsafeIOToST $ liftM realToFrac $ f m 

setFloat :: (Real r, Fractional f) => (Ptr CDdManager -> f -> IO ()) -> STDdManager s u -> r -> ST s ()
setFloat f (STDdManager m) v = unsafeIOToST $ f m (realToFrac v)

--Reordering types
{#enum Cudd_ReorderingType as CuddReorderingType {underscoreToCase} deriving (Show, Eq) #}

--Reorder when needed
foreign import ccall safe "cudd.h Cudd_ReorderingStatus"
	c_cuddReorderingStatus :: Ptr CDdManager -> Ptr CInt -> IO (CInt)

cuddReorderingStatus :: STDdManager s u -> ST s (Int, CuddReorderingType)
cuddReorderingStatus (STDdManager m) = unsafeIOToST $ do
	alloca $ \mem -> do
		res <- c_cuddReorderingStatus m mem
		typ <- peek mem
		return $ (fromIntegral res, toEnum $ fromIntegral typ)

foreign import ccall safe "cudd.h Cudd_AutodynEnable"
	c_cuddAutodynEnable :: Ptr CDdManager -> CInt -> IO ()

cuddAutodynEnable :: STDdManager s u -> CuddReorderingType -> ST s ()
cuddAutodynEnable (STDdManager m) t = unsafeIOToST $ c_cuddAutodynEnable m (fromIntegral $ fromEnum t)

foreign import ccall safe "cudd.h Cudd_AutodynDisable"
	c_cuddAutodynDisable :: Ptr CDdManager -> IO ()

cuddAutodynDisable :: STDdManager s u -> ST s ()
cuddAutodynDisable (STDdManager m) = unsafeIOToST $ c_cuddAutodynDisable m

--Reorder right now
foreign import ccall safe "cudd.h Cudd_ReduceHeap"
	c_cuddReduceHeap :: Ptr CDdManager -> CInt -> CInt -> IO (CInt)

cuddReduceHeap :: STDdManager s u -> CuddReorderingType -> Int -> ST s Int
cuddReduceHeap (STDdManager m) typ minsize = unsafeIOToST $ liftM fromIntegral $ c_cuddReduceHeap m (fromIntegral $ fromEnum typ) (fromIntegral minsize)

--Grouping
foreign import ccall safe "cudd.h Cudd_MakeTreeNode"
	c_cuddMakeTreeNode :: Ptr CDdManager -> CUInt -> CUInt -> CUInt -> IO (Ptr ())

cuddMakeTreeNode :: STDdManager s u -> Int -> Int -> Int -> ST s (Ptr ())
cuddMakeTreeNode (STDdManager m) low size typ = unsafeIOToST $ do
    res <- c_cuddMakeTreeNode m (fromIntegral low) (fromIntegral size) (fromIntegral typ)
    when (res==nullPtr) (error "cuddMakeTreeNode returned error")
    return res

--Reordering stats
foreign import ccall safe "cudd.h Cudd_ReadReorderingTime"
	c_cuddReadReorderingTime :: Ptr CDdManager -> IO (CLong)

cuddReadReorderingTime :: STDdManager s u -> ST s Int
cuddReadReorderingTime = readIntegral c_cuddReadReorderingTime

foreign import ccall safe "cudd.h Cudd_ReadReorderings"
	c_cuddReadReorderings :: Ptr CDdManager -> IO (CUInt)

cuddReadReorderings :: STDdManager s u -> ST s Int
cuddReadReorderings = readIntegral c_cuddReadReorderings

--Hooks
foreign import ccall safe "cudd.h &Cudd_StdPreReordHook"
	c_cuddStdPreReordHook :: HookFP

foreign import ccall safe "cudd.h &Cudd_StdPostReordHook"
	c_cuddStdPostReordHook :: HookFP

foreign import ccall safe "cudd.h Cudd_EnableReorderingReporting"
	c_cuddEnableReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddEnableReorderingReporting :: STDdManager s u -> ST s Int
cuddEnableReorderingReporting (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddEnableReorderingReporting m

foreign import ccall safe "cudd.h Cudd_DisableReorderingReporting"
	c_cuddDisableReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddDisableReorderingReporting :: STDdManager s u -> ST s Int
cuddDisableReorderingReporting (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddDisableReorderingReporting m

foreign import ccall safe "cudd.h Cudd_ReorderingReporting"
	c_cuddReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddReorderingReporting :: STDdManager s u -> ST s Int
cuddReorderingReporting (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddReorderingReporting m

regStdPreReordHook :: STDdManager s u -> ST s Int
regStdPreReordHook m = cuddAddHook m c_cuddStdPreReordHook CuddPreReorderingHook

regStdPostReordHook :: STDdManager s u -> ST s Int
regStdPostReordHook m = cuddAddHook m c_cuddStdPostReordHook CuddPostReorderingHook

--Universal reordering params
foreign import ccall safe "cudd.h Cudd_TurnOffCountDead"
	c_cuddTurnOffCountDead :: Ptr CDdManager -> IO ()

cuddTurnOffCountDead :: STDdManager s u -> ST s ()
cuddTurnOffCountDead (STDdManager m) = unsafeIOToST $ c_cuddTurnOffCountDead m

foreign import ccall safe "cudd.h Cudd_TurnOnCountDead"
	c_cuddTurnOnCountDead :: Ptr CDdManager -> IO ()

cuddTurnOnCountDead :: STDdManager s u -> ST s ()
cuddTurnOnCountDead (STDdManager m) = unsafeIOToST $ c_cuddTurnOnCountDead m

foreign import ccall safe "cudd.h Cudd_DeadAreCounted"
	c_cuddDeadAreCounted :: Ptr CDdManager -> IO CInt

cuddDeadAreCounted :: STDdManager s u -> ST s Int
cuddDeadAreCounted (STDdManager m) = unsafeIOToST $ liftM fromIntegral $ c_cuddDeadAreCounted m

--Sifting parameters
foreign import ccall safe "cudd.h Cudd_ReadSiftMaxSwap"
	c_cuddReadSiftMaxSwap :: Ptr CDdManager -> IO (CInt)

cuddReadSiftMaxSwap :: STDdManager s u -> ST s Int
cuddReadSiftMaxSwap = readIntegral c_cuddReadSiftMaxSwap

foreign import ccall safe "cudd.h Cudd_SetSiftMaxSwap"
	c_cuddSetSiftMaxSwap :: Ptr CDdManager -> CInt -> IO ()

cuddSetSiftMaxSwap :: STDdManager s u -> Int -> ST s ()
cuddSetSiftMaxSwap = setIntegral c_cuddSetSiftMaxSwap 

foreign import ccall safe "cudd.h Cudd_ReadSiftMaxVar"
	c_cuddReadSiftMaxVar :: Ptr CDdManager -> IO (Int)

cuddReadSiftMaxVar :: STDdManager s u -> ST s Int
cuddReadSiftMaxVar = readIntegral c_cuddReadSiftMaxVar

foreign import ccall safe "cudd.h Cudd_SetSiftMaxVar"
	c_cuddSetSiftMaxVar :: Ptr CDdManager -> CInt -> IO ()

cuddSetSiftMaxVar :: STDdManager s u -> Int -> ST s ()
cuddSetSiftMaxVar = setIntegral c_cuddSetSiftMaxVar
	
foreign import ccall safe "cudd.h Cudd_ReadNextReordering"
	c_cuddReadNextReordering :: Ptr CDdManager -> IO (Int)

cuddReadNextReordering :: STDdManager s u -> ST s Int
cuddReadNextReordering = readIntegral c_cuddReadNextReordering

foreign import ccall safe "cudd.h Cudd_SetNextReordering"
	c_cuddSetNextReordering :: Ptr CDdManager -> CInt -> IO ()

cuddSetNextReordering :: STDdManager s u -> Int -> ST s ()
cuddSetNextReordering = setIntegral c_cuddSetNextReordering

foreign import ccall safe "cudd.h Cudd_ReadMaxGrowthAlternate"
	c_cuddReadMaxGrowthAlternate :: Ptr CDdManager -> IO CDouble

cuddReadMaxGrowthAlternate :: STDdManager s u -> ST s Double
cuddReadMaxGrowthAlternate = readFloat c_cuddReadMaxGrowthAlternate

foreign import ccall safe "cudd.h Cudd_SetMaxGrowthAlternate"
	c_cuddSetMaxGrowthAlternate :: Ptr CDdManager -> CDouble -> IO ()

cuddSetMaxGrowthAlternate :: STDdManager s u -> Double -> ST s ()
cuddSetMaxGrowthAlternate = setFloat c_cuddSetMaxGrowthAlternate

foreign import ccall safe "cudd.h Cudd_ReadMaxGrowth"
	c_cuddReadMaxGrowth :: Ptr CDdManager -> IO CDouble

cuddReadMaxGrowth :: STDdManager s u -> ST s Double
cuddReadMaxGrowth = readFloat c_cuddReadMaxGrowth

foreign import ccall safe "cudd.h Cudd_SetMaxGrowth"
	c_cuddSetMaxGrowth :: Ptr CDdManager -> CDouble -> IO ()

cuddSetMaxGrowth :: STDdManager s u -> Double -> ST s ()
cuddSetMaxGrowth = setFloat c_cuddSetMaxGrowth

foreign import ccall safe "cudd.h Cudd_ReadReorderingCycle"
	c_cuddReadReorderingCycle :: Ptr CDdManager -> IO (CInt)

cuddReadReorderingCycle :: STDdManager s u -> ST s Int
cuddReadReorderingCycle = readIntegral c_cuddReadReorderingCycle

foreign import ccall safe "cudd.h Cudd_SetReorderingCycle"
	c_cuddSetReorderingCycle :: Ptr CDdManager -> CInt -> IO ()

cuddSetReorderingCycle :: STDdManager s u -> Int -> ST s ()
cuddSetReorderingCycle = setIntegral c_cuddSetReorderingCycle

--Genetic algorithm
foreign import ccall safe "cudd.h Cudd_ReadPopulationSize"
	c_cuddReadPopulationSize :: Ptr CDdManager -> IO (CInt)

cuddReadPopulationSize :: STDdManager s u -> ST s Int
cuddReadPopulationSize = readIntegral c_cuddReadPopulationSize

foreign import ccall safe "cudd.h Cudd_SetPopulationSize"
	c_cuddSetPopulationSize :: Ptr CDdManager -> CInt -> IO ()

cuddSetPopulationSize :: STDdManager s u -> Int -> ST s ()
cuddSetPopulationSize = setIntegral c_cuddSetPopulationSize

foreign import ccall safe "cudd.h Cudd_ReadNumberXovers"
	c_cuddReadNumberXovers :: Ptr CDdManager -> IO (CInt)

cuddReadNumberXovers :: STDdManager s u -> ST s Int
cuddReadNumberXovers = readIntegral c_cuddReadNumberXovers

foreign import ccall safe "cudd.h Cudd_SetNumberXovers"
	c_cuddSetNumberXovers :: Ptr CDdManager -> CInt -> IO ()

cuddSetNumberXovers :: STDdManager s u -> Int -> ST s ()
cuddSetNumberXovers = setIntegral c_cuddSetNumberXovers

reordGCHook :: HookTyp
reordGCHook _ _ _ = do
    --putStrLn "reordGCHook"
    performGC
    --putStrLn "gc done"
    return (fromIntegral 1)

foreign import ccall "wrapper"
    makeFunPtr :: HookTyp -> IO (FunPtr HookTyp)

regReordGCHook :: STDdManager s u -> ST s Int
regReordGCHook m = do
    hk <- unsafeIOToST $ makeFunPtr reordGCHook
    cuddAddHook m hk CuddPreReorderingHook

