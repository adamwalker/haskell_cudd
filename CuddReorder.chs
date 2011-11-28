{-#LANGUAGE ForeignFunctionInterface #-}

module CuddReorder where

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
import CuddHook

#include <stdio.h>
#include <cudd.h>

readIntegral :: (Integral i, Num j) => (Ptr CDdManager -> IO i) -> DdManager -> IO j
readIntegral f (DdManager m) = liftM fromIntegral $ f m

setIntegral :: (Integral i, Num j) => (Ptr CDdManager -> j -> IO ()) -> DdManager -> i -> IO ()
setIntegral f (DdManager m) v = f m (fromIntegral v)

readFloat :: (Real r, Fractional f) => (Ptr CDdManager -> IO r) -> DdManager -> IO f
readFloat f (DdManager m) = liftM realToFrac $ f m 

setFloat :: (Real r, Fractional f) => (Ptr CDdManager -> f -> IO ()) -> DdManager -> r -> IO ()
setFloat f (DdManager m) v = f m (realToFrac v)

--Reordering types
{#enum Cudd_ReorderingType as CuddReorderingType {underscoreToCase} deriving (Show, Eq) #}

--Reorder when needed
foreign import ccall unsafe "cudd.h Cudd_ReorderingStatus"
	c_cuddReorderingStatus :: Ptr CDdManager -> Ptr CInt -> IO (CInt)

cuddReorderingStatus :: DdManager -> IO (Int, CuddReorderingType)
cuddReorderingStatus (DdManager m) = do
	alloca $ \mem -> do
		res <- c_cuddReorderingStatus m mem
		typ <- peek mem
		return $ (fromIntegral res, toEnum $ fromIntegral typ)

foreign import ccall unsafe "cudd.h Cudd_AutodynEnable"
	c_cuddAutodynEnable :: Ptr CDdManager -> CInt -> IO ()

cuddAutodynEnable :: DdManager -> CuddReorderingType -> IO ()
cuddAutodynEnable (DdManager m) t = c_cuddAutodynEnable m (fromIntegral $ fromEnum t)

--Reorder right now
foreign import ccall unsafe "cudd.h Cudd_ReduceHeap"
	c_cuddReduceHeap :: Ptr CDdManager -> CInt -> CInt -> IO (CInt)

cuddReduceHeap :: DdManager -> CuddReorderingType -> Int -> IO (Int)
cuddReduceHeap (DdManager m) typ minsize = liftM fromIntegral $ c_cuddReduceHeap m (fromIntegral $ fromEnum typ) (fromIntegral minsize)

--Grouping
foreign import ccall unsafe "cudd.h Cudd_MakeTreeNode"
	c_cuddMakeTreeNode :: Ptr CDdManager -> CUInt -> CUInt -> CUInt -> IO (Ptr ())

cuddMakeTreeNode :: DdManager -> Int -> Int -> Int -> IO (Ptr ())
cuddMakeTreeNode (DdManager m) low size typ = do
    res <- c_cuddMakeTreeNode m (fromIntegral low) (fromIntegral size) (fromIntegral typ)
    when (res==nullPtr) (error "cuddMakeTreeNode returned error")
    return res

--Reordering stats
foreign import ccall unsafe "cudd.h Cudd_ReadReorderingTime"
	c_cuddReadReorderingTime :: Ptr CDdManager -> IO (CLong)

cuddReadReorderingTime :: DdManager -> IO (Int)
cuddReadReorderingTime = readIntegral c_cuddReadReorderingTime

foreign import ccall unsafe "cudd.h Cudd_ReadReorderings"
	c_cuddReadReorderings :: Ptr CDdManager -> IO (CInt)

cuddReadReorderings :: DdManager -> IO (Int)
cuddReadReorderings = readIntegral c_cuddReadReorderings

--Hooks
foreign import ccall unsafe "cudd.h &Cudd_StdPreReordHook"
	c_cuddStdPreReordHook :: HookFP

foreign import ccall unsafe "cudd.h &Cudd_StdPostReordHook"
	c_cuddStdPostReordHook :: HookFP

foreign import ccall unsafe "cudd.h Cudd_EnableReorderingReporting"
	c_cuddEnableReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddEnableReorderingReporting :: DdManager -> IO (Int)
cuddEnableReorderingReporting (DdManager m) = liftM fromIntegral $ c_cuddEnableReorderingReporting m

foreign import ccall unsafe "cudd.h Cudd_DisableReorderingReporting"
	c_cuddDisableReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddDisableReorderingReporting :: DdManager -> IO (Int)
cuddDisableReorderingReporting (DdManager m) = liftM fromIntegral $ c_cuddDisableReorderingReporting m

foreign import ccall unsafe "cudd.h Cudd_ReorderingReporting"
	c_cuddReorderingReporting :: Ptr CDdManager -> IO (CInt)

cuddReorderingReporting :: DdManager -> IO (Int)
cuddReorderingReporting (DdManager m) = liftM fromIntegral $ c_cuddReorderingReporting m

regStdPreReordHook :: DdManager -> IO (Int)
regStdPreReordHook m = cuddAddHook m c_cuddStdPreReordHook CuddPreReorderingHook

regStdPostReordHook :: DdManager -> IO (Int)
regStdPostReordHook m = cuddAddHook m c_cuddStdPostReordHook CuddPostReorderingHook

--Universal reordering params
foreign import ccall unsafe "cudd.h Cudd_TurnOffCountDead"
	c_cuddTurnOffCountDead :: Ptr CDdManager -> IO ()

cuddturnOffCountDead :: DdManager -> IO ()
cuddturnOffCountDead (DdManager m) = c_cuddTurnOffCountDead m

foreign import ccall unsafe "cudd.h Cudd_TurnOnCountDead"
	c_cuddTurnOnCountDead :: Ptr CDdManager -> IO ()

cuddTurnOnCountDead :: DdManager -> IO ()
cuddTurnOnCountDead (DdManager m) = c_cuddTurnOnCountDead m

foreign import ccall unsafe "cudd.h Cudd_DeadAreCounted"
	c_cuddDeadAreCounted :: Ptr CDdManager -> IO CInt

cuddDeadAreCounted :: DdManager -> IO (Int)
cuddDeadAreCounted (DdManager m) = liftM fromIntegral $ c_cuddDeadAreCounted m

--Sifting parameters
foreign import ccall unsafe "cudd.h Cudd_ReadSiftMaxSwap"
	c_cuddReadSiftMaxSwap :: Ptr CDdManager -> IO (CInt)

cuddReadSiftMaxSwap :: DdManager -> IO (Int)
cuddReadSiftMaxSwap = readIntegral c_cuddReadSiftMaxSwap

foreign import ccall unsafe "cudd.h Cudd_SetSiftMaxSwap"
	c_cuddSetSiftMaxSwap :: Ptr CDdManager -> CInt -> IO ()

cuddSetSiftMaxSwap :: DdManager -> Int -> IO ()
cuddSetSiftMaxSwap = setIntegral c_cuddSetSiftMaxSwap 

foreign import ccall unsafe "cudd.h Cudd_ReadSiftMaxVar"
	c_cuddReadSiftMaxVar :: Ptr CDdManager -> IO (Int)

cuddReadSiftMaxVar :: DdManager -> IO (Int)
cuddReadSiftMaxVar = readIntegral c_cuddReadSiftMaxVar

foreign import ccall unsafe "cudd.h Cudd_SetSiftMaxVar"
	c_cuddSetSiftMaxVar :: Ptr CDdManager -> CInt -> IO ()

cuddSetsiftMaxVar :: DdManager -> Int -> IO ()
cuddSetsiftMaxVar = setIntegral c_cuddSetSiftMaxVar
	
foreign import ccall unsafe "cudd.h Cudd_ReadNextReordering"
	c_cuddReadNextReordering :: Ptr CDdManager -> IO (Int)

cuddReadNextReordering :: DdManager -> IO (Int)
cuddReadNextReordering = readIntegral c_cuddReadNextReordering

foreign import ccall unsafe "cudd.h Cudd_SetNextReordering"
	c_cuddSetNextReordering :: Ptr CDdManager -> CInt -> IO ()

cuddSetNextReordering :: DdManager -> Int -> IO ()
cuddSetNextReordering = setIntegral c_cuddSetNextReordering

foreign import ccall unsafe "cudd.h Cudd_ReadMaxGrowthAlternate"
	c_cuddReadMaxGrowthAlternate :: Ptr CDdManager -> IO CDouble

cuddReadMaxGrowthAlternate :: DdManager -> IO (Double)
cuddReadMaxGrowthAlternate = readFloat c_cuddReadMaxGrowthAlternate

foreign import ccall unsafe "cudd.h Cudd_SetMaxGrowthAlternate"
	c_cuddSetMaxGrowthAlternate :: Ptr CDdManager -> CDouble -> IO ()

cuddSetMaxGrowthAlternate :: DdManager -> Double -> IO ()
cuddSetMaxGrowthAlternate = setFloat c_cuddSetMaxGrowthAlternate

foreign import ccall unsafe "cudd.h Cudd_ReadMaxGrowth"
	c_cuddReadMaxGrowth :: Ptr CDdManager -> IO CDouble

cuddReadMaxGrowth :: DdManager -> IO CDouble
cuddReadMaxGrowth = readFloat c_cuddReadMaxGrowth

foreign import ccall unsafe "cudd.h Cudd_SetMaxGrowth"
	c_cuddSetMaxGrowth :: Ptr CDdManager -> CDouble -> IO ()

cuddSetMaxGrowth :: DdManager -> Double -> IO ()
cuddSetMaxGrowth = setFloat c_cuddSetMaxGrowth

foreign import ccall unsafe "cudd.h Cudd_ReadReorderingCycle"
	c_cuddReadReorderingCycle :: Ptr CDdManager -> IO (CInt)

cuddReadReorderingCycle :: DdManager -> IO Int
cuddReadReorderingCycle = readIntegral c_cuddReadReorderingCycle

foreign import ccall unsafe "cudd.h Cudd_SetReorderingCycle"
	c_cuddSetReorderingCycle :: Ptr CDdManager -> CInt -> IO ()

cuddSetReorderingCycle :: DdManager -> CInt -> IO ()
cuddSetReorderingCycle = setIntegral c_cuddSetReorderingCycle

--Genetic algorithm
foreign import ccall unsafe "cudd.h Cudd_ReadPopulationSize"
	c_cuddReadPopulationSize :: Ptr CDdManager -> IO (CInt)

cuddReadPopulationSize :: DdManager -> IO (Int)
cuddReadPopulationSize = readIntegral c_cuddReadPopulationSize

foreign import ccall unsafe "cudd.h Cudd_SetPopulationSize"
	c_cuddSetPopulationSize :: Ptr CDdManager -> CInt -> IO ()

cuddSetPopulationSize :: DdManager -> Int -> IO ()
cuddSetPopulationSize = setIntegral c_cuddSetPopulationSize

foreign import ccall unsafe "cudd.h Cudd_ReadNumberXovers"
	c_cuddReadNumberXovers :: Ptr CDdManager -> IO (CInt)

cuddReadNumberXovers :: DdManager -> IO (Int)
cuddReadNumberXovers = readIntegral c_cuddReadNumberXovers

foreign import ccall unsafe "cudd.h Cudd_SetNumberXovers"
	c_cuddSetNumberXovers :: Ptr CDdManager -> CInt -> IO ()

cuddSetNumberXovers :: DdManager -> Int -> IO ()
cuddSetNumberXovers = setIntegral c_cuddSetNumberXovers

