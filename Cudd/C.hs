{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}

module Cudd.C (
    CDdManager,
    CDdNode,
    c_cuddReadOne,
    c_cuddReadLogicZero,
    c_cuddReadOneWithRef,
    c_cuddReadLogicZeroWithRef,
    c_cuddBddIthVar,
    c_cuddBddAnd,
    c_cuddBddOr,
    c_cuddBddNand,
    c_cuddBddNor,
    c_cuddBddXor,
    c_cuddBddXnor,
    c_cuddNot,
    c_cuddNotNoRef,
    c_cuddBddIte,
    c_cuddBddExistAbstract,
    c_cuddBddUnivAbstract,
    c_cuddIterDerefBdd,
    cuddRef,
    c_cuddInit,
    c_cuddShuffleHeap,
    c_cuddSetVarMap,
    c_cuddBddVarMap,
    c_cuddBddLeq,
    c_cuddBddSwapVariables,
    c_cuddLargestCube,
    c_cuddBddMakePrime,
    c_cuddSupport,
    c_cuddSupportIndex,
    c_cuddSupportIndices,
    c_cuddIndicesToCube,
    c_cuddBddComputeCube,
    c_cuddBddToCubeArray,
    c_cuddReadSize,
    c_cuddBddCompose,
    c_cuddBddAndAbstract,
    c_cuddBddXorExistAbstract,
    c_cuddBddLeqUnless,
    c_cuddEquivDC,
    c_cuddXeqy,
    c_cuddDebugCheck,
    c_cuddCheckKeys,
    c_cuddBddPickOneMinterm,
    c_cuddCheckZeroRef,
    c_cuddReadInvPerm,
    c_cuddReadPerm,
    c_cuddDagSize,
    c_cuddReadNodeCount,
    c_cuddReadPeakNodeCount,
    c_cuddReadMaxCache,
    c_cuddReadMaxCacheHard,
    c_cuddSetMaxCacheHard,
    c_cuddReadCacheSlots,
    c_cuddReadCacheUsedSlots,
    c_cuddBddAndLimit,
    c_cuddBddNewVarAtLevel,
    c_cuddReadTree,
    c_cuddBddLICompaction,
    c_cuddBddSqueeze,
    c_cuddBddMinimize,
    c_cuddEval,
    c_cuddCountLeaves,
    c_cuddCountMinterm,
    c_cuddCountPathsToNonZero,
    c_cuddCountPath,
    c_cuddBddConstrain,
    c_cuddBddRestrict,
    c_wrappedRegular,
    c_cuddBddPermute,
    c_cuddXgty,
    c_cuddInequality,
    c_cuddDisequality,
    c_cuddBddInterval,
    c_cuddNodeReadIndex,
    c_cuddBddTransfer,
    c_cuddRecursiveDerefPtr,
    c_cuddDelayedDerefBddPtr,
    c_cuddIterDerefBddPtr,
    c_cuddBddNewVar,
    c_cuddBddVectorCompose
    ) where

import Foreign
import Foreign.C.Types

import Cudd.MTR

data CDdManager
data CDdNode

foreign import ccall safe "cudd.h Cudd_ReadOne_s"
	c_cuddReadOne :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_ReadLogicZero_s"
	c_cuddReadLogicZero :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_ReadOne_withRef_s"
	c_cuddReadOneWithRef :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_ReadLogicZero_withRef_s"
	c_cuddReadLogicZeroWithRef :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddIthVar_s"
	c_cuddBddIthVar :: Ptr CDdManager -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddAnd_s"
	c_cuddBddAnd :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddOr_s"
	c_cuddBddOr :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddNand_s"
	c_cuddBddNand :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddNor_s"
	c_cuddBddNor :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddXor_s"
	c_cuddBddXor :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddXnor_s"
	c_cuddBddXnor :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Not_s"
	c_cuddNot :: Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_NotNoRef_s"
	c_cuddNotNoRef :: Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddIte_s"
    c_cuddBddIte :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddExistAbstract_s"
	c_cuddBddExistAbstract :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddUnivAbstract_s"
	c_cuddBddUnivAbstract :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_IterDerefBdd"
	c_cuddIterDerefBdd :: Ptr CDdManager -> Ptr CDdNode -> IO ()

foreign import ccall safe "cuddwrap.h wrappedCuddRef"
	cuddRef :: Ptr CDdNode -> IO ()

foreign import ccall safe "cudd.h Cudd_Init"
	c_cuddInit :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CDdManager)

foreign import ccall safe "cudd.h Cudd_ShuffleHeap"
    c_cuddShuffleHeap :: Ptr CDdManager -> Ptr CInt -> IO CInt

foreign import ccall safe "cudd.h Cudd_SetVarMap"
    c_cuddSetVarMap :: Ptr CDdManager -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> CInt -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddVarMap_s"
    c_cuddBddVarMap :: Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddLeq"
    c_cuddBddLeq :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddSwapVariables_s"
    c_cuddBddSwapVariables :: Ptr CDdManager -> Ptr CDdNode -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_LargestCube_s"
    c_cuddLargestCube :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddMakePrime_s"
    c_cuddBddMakePrime :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Support_s"
    c_cuddSupport :: Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_SupportIndex"
	c_cuddSupportIndex :: Ptr CDdManager -> Ptr CDdNode -> IO(Ptr CInt)

foreign import ccall safe "cudd.h Cudd_SupportIndices"
    c_cuddSupportIndices :: Ptr CDdManager -> Ptr CDdNode -> Ptr (Ptr CInt) -> IO CInt

foreign import ccall safe "cudd.h Cudd_IndicesToCube_s"
    c_cuddIndicesToCube :: Ptr CDdManager -> Ptr CInt -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddComputeCube_s"
    c_cuddBddComputeCube :: Ptr CDdManager -> Ptr (Ptr CDdNode) -> Ptr CInt -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_BddToCubeArray"
    c_cuddBddToCubeArray :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadSize"
	c_cuddReadSize :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddCompose_s"
    c_cuddBddCompose :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddAndAbstract_s"
    c_cuddBddAndAbstract :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddXorExistAbstract_s"
    c_cuddBddXorExistAbstract :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddLeqUnless"
    c_cuddBddLeqUnless :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_EquivDC"
    c_cuddEquivDC :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_Xeqy_s"
	c_cuddXeqy :: Ptr CDdManager -> CInt -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_DebugCheck"
    c_cuddDebugCheck :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_CheckKeys"
    c_cuddCheckKeys :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddPickOneMinterm_s"
	c_cuddBddPickOneMinterm :: Ptr CDdManager -> Ptr CDdNode -> Ptr (Ptr CDdNode) -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_CheckZeroRef"
    c_cuddCheckZeroRef :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadInvPerm"
    c_cuddReadInvPerm :: Ptr CDdManager -> CInt -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadPerm"
    c_cuddReadPerm :: Ptr CDdManager -> CInt -> IO CInt

foreign import ccall safe "cudd.h Cudd_DagSize"
    c_cuddDagSize :: Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadNodeCount"
    c_cuddReadNodeCount :: Ptr CDdManager -> IO CLong

foreign import ccall safe "cudd.h Cudd_ReadPeakNodeCount"
    c_cuddReadPeakNodeCount :: Ptr CDdManager -> IO CLong

foreign import ccall safe "cudd.h Cudd_ReadMaxCache"
    c_cuddReadMaxCache :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadMaxCacheHard"
    c_cuddReadMaxCacheHard :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_SetMaxCacheHard"
    c_cuddSetMaxCacheHard :: Ptr CDdManager -> CInt -> IO ()

foreign import ccall safe "cudd.h Cudd_ReadCacheSlots"
    c_cuddReadCacheSlots :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_ReadCacheUsedSlots"
    c_cuddReadCacheUsedSlots :: Ptr CDdManager -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddAndLimit"
    c_cuddBddAndLimit :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> CUInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddNewVarAtLevel_s"
    c_cuddBddNewVarAtLevel :: Ptr CDdManager -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_ReadTree"
    c_cuddReadTree :: Ptr CDdManager -> IO (Ptr CMtrNode)

foreign import ccall safe "cudd.h Cudd_bddLICompaction_s"
    c_cuddBddLICompaction :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddSqueeze_s" 
    c_cuddBddSqueeze :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddMinimize_s"
    c_cuddBddMinimize :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Eval"
    c_cuddEval :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd,h Cudd_CountLeaves"
    c_cuddCountLeaves :: Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_CountMinterm"
    c_cuddCountMinterm :: Ptr CDdManager -> Ptr CDdNode -> CInt -> IO CDouble

foreign import ccall safe "cudd.h Cudd_CountPathsToNonZero"
    c_cuddCountPathsToNonZero :: Ptr CDdNode -> IO CDouble

foreign import ccall safe "cudd.h Cudd_CountPath"
    c_cuddCountPath :: Ptr CDdNode -> IO CDouble

foreign import ccall safe "cudd.h Cudd_bddConstrain_s"
    c_cuddBddConstrain :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddRestrict_s"
    c_cuddBddRestrict :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h wrappedRegular"
    c_wrappedRegular :: Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddPermute_s"
    c_cuddBddPermute :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Xgty_s"
	c_cuddXgty :: Ptr CDdManager -> CInt -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Inequality_s"
	c_cuddInequality :: Ptr CDdManager -> CInt -> CInt -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_Disequality_s"
	c_cuddDisequality :: Ptr CDdManager -> CInt -> CInt -> Ptr (Ptr CDdNode) -> Ptr (Ptr CDdNode) -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddInterval_s"
    c_cuddBddInterval :: Ptr CDdManager -> CInt -> Ptr (Ptr CDdNode) -> CInt -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_NodeReadIndex"
    c_cuddNodeReadIndex :: Ptr CDdNode -> IO CInt

foreign import ccall safe "cudd.h Cudd_bddTransfer"
    c_cuddBddTransfer :: Ptr CDdManager -> Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h &Cudd_RecursiveDeref"
	c_cuddRecursiveDerefPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_DelayedDerefBdd"
	c_cuddDelayedDerefBddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_IterDerefBdd"
	c_cuddIterDerefBddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h Cudd_bddNewVar_s"
    c_cuddBddNewVar :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddVectorCompose_s"
    c_cuddBddVectorCompose :: Ptr CDdManager -> Ptr CDdNode -> Ptr (Ptr CDdNode) -> IO (Ptr CDdNode) 

