{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}

module Cudd.C (
    CDDManager,
    CDDNode,
    CDDGen,
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
    c_cuddApaCountMinterm,
    c_cuddFreeApaNumber,
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
    c_cuddBddVectorCompose,
    c_cuddQuit,
    c_cuddPrintMinterm,
    c_cuddCheckCube,
    c_cuddPrintInfo,
    c_cuddPrintDebug,
    c_cuddIsComplement,
    c_cuddDumpDot,
    c_cuddFirstCube,
    c_cuddNextCube,
    c_cuddFirstPrime,
    c_cuddNextPrime,
    c_cuddIsGenEmpty,
    c_cuddGenFree,
    c_cuddFirstNode,
    c_cuddNextNode
    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Cudd.MTR

data CDDManager
data CDDNode
data CDDGen

foreign import ccall safe "Cudd_ReadOne_s"
	c_cuddReadOne :: Ptr CDDManager -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_ReadLogicZero_s"
	c_cuddReadLogicZero :: Ptr CDDManager -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_ReadOne_withRef_s"
	c_cuddReadOneWithRef :: Ptr CDDManager -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_ReadLogicZero_withRef_s"
	c_cuddReadLogicZeroWithRef :: Ptr CDDManager -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddIthVar_s"
	c_cuddBddIthVar :: Ptr CDDManager -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddAnd_s"
	c_cuddBddAnd :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddOr_s"
	c_cuddBddOr :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddNand_s"
	c_cuddBddNand :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddNor_s"
	c_cuddBddNor :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddXor_s"
	c_cuddBddXor :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddXnor_s"
	c_cuddBddXnor :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Not_s"
	c_cuddNot :: Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_NotNoRef_s"
	c_cuddNotNoRef :: Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddIte_s"
    c_cuddBddIte :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddExistAbstract_s"
	c_cuddBddExistAbstract :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddUnivAbstract_s"
	c_cuddBddUnivAbstract :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_IterDerefBdd"
	c_cuddIterDerefBdd :: Ptr CDDManager -> Ptr CDDNode -> IO ()

foreign import ccall safe "wrappedCuddRef"
	cuddRef :: Ptr CDDNode -> IO ()

foreign import ccall safe "Cudd_Init"
	c_cuddInit :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CDDManager)

foreign import ccall safe "Cudd_ShuffleHeap"
    c_cuddShuffleHeap :: Ptr CDDManager -> Ptr CInt -> IO CInt

foreign import ccall safe "Cudd_SetVarMap"
    c_cuddSetVarMap :: Ptr CDDManager -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> CInt -> IO CInt

foreign import ccall safe "Cudd_bddVarMap_s"
    c_cuddBddVarMap :: Ptr CDDManager -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddLeq"
    c_cuddBddLeq :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_bddSwapVariables_s"
    c_cuddBddSwapVariables :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_LargestCube_s"
    c_cuddLargestCube :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddMakePrime_s"
    c_cuddBddMakePrime :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Support_s"
    c_cuddSupport :: Ptr CDDManager -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_SupportIndex"
	c_cuddSupportIndex :: Ptr CDDManager -> Ptr CDDNode -> IO(Ptr CInt)

foreign import ccall safe "Cudd_SupportIndices"
    c_cuddSupportIndices :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CInt) -> IO CInt

foreign import ccall safe "Cudd_IndicesToCube_s"
    c_cuddIndicesToCube :: Ptr CDDManager -> Ptr CInt -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddComputeCube_s"
    c_cuddBddComputeCube :: Ptr CDDManager -> Ptr (Ptr CDDNode) -> Ptr CInt -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_BddToCubeArray"
    c_cuddBddToCubeArray :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> IO CInt

foreign import ccall safe "Cudd_ReadSize"
	c_cuddReadSize :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_bddCompose_s"
    c_cuddBddCompose :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddAndAbstract_s"
    c_cuddBddAndAbstract :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddXorExistAbstract_s"
    c_cuddBddXorExistAbstract :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddLeqUnless"
    c_cuddBddLeqUnless :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_EquivDC"
    c_cuddEquivDC :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_Xeqy_s"
	c_cuddXeqy :: Ptr CDDManager -> CInt -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_DebugCheck"
    c_cuddDebugCheck :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_CheckKeys"
    c_cuddCheckKeys :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_bddPickOneMinterm_s"
	c_cuddBddPickOneMinterm :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CDDNode) -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_CheckZeroRef"
    c_cuddCheckZeroRef :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_ReadInvPerm"
    c_cuddReadInvPerm :: Ptr CDDManager -> CInt -> IO CInt

foreign import ccall safe "Cudd_ReadPerm"
    c_cuddReadPerm :: Ptr CDDManager -> CInt -> IO CInt

foreign import ccall safe "Cudd_DagSize"
    c_cuddDagSize :: Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_ReadNodeCount"
    c_cuddReadNodeCount :: Ptr CDDManager -> IO CLong

foreign import ccall safe "Cudd_ReadPeakNodeCount"
    c_cuddReadPeakNodeCount :: Ptr CDDManager -> IO CLong

foreign import ccall safe "Cudd_ReadMaxCache"
    c_cuddReadMaxCache :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_ReadMaxCacheHard"
    c_cuddReadMaxCacheHard :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_SetMaxCacheHard"
    c_cuddSetMaxCacheHard :: Ptr CDDManager -> CInt -> IO ()

foreign import ccall safe "Cudd_ReadCacheSlots"
    c_cuddReadCacheSlots :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_ReadCacheUsedSlots"
    c_cuddReadCacheUsedSlots :: Ptr CDDManager -> IO CInt

foreign import ccall safe "Cudd_bddAndLimit"
    c_cuddBddAndLimit :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> CUInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddNewVarAtLevel_s"
    c_cuddBddNewVarAtLevel :: Ptr CDDManager -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_ReadTree"
    c_cuddReadTree :: Ptr CDDManager -> IO (Ptr CMtrNode)

foreign import ccall safe "Cudd_bddLICompaction_s"
    c_cuddBddLICompaction :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddSqueeze_s" 
    c_cuddBddSqueeze :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddMinimize_s"
    c_cuddBddMinimize :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Eval"
    c_cuddEval :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> IO (Ptr CDDNode)

foreign import ccall safe ",h Cudd_CountLeaves"
    c_cuddCountLeaves :: Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_CountMinterm"
    c_cuddCountMinterm :: Ptr CDDManager -> Ptr CDDNode -> CInt -> IO CDouble

foreign import ccall safe "Cudd_FreeApaNumber"
    c_cuddFreeApaNumber :: Ptr CInt -> IO () 

foreign import ccall safe "Cudd_ApaCountMinterm"
    c_cuddApaCountMinterm :: Ptr CDDManager -> Ptr CDDNode -> CInt -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall safe "Cudd_CountPathsToNonZero"
    c_cuddCountPathsToNonZero :: Ptr CDDNode -> IO CDouble

foreign import ccall safe "Cudd_CountPath"
    c_cuddCountPath :: Ptr CDDNode -> IO CDouble

foreign import ccall safe "Cudd_bddConstrain_s"
    c_cuddBddConstrain :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddRestrict_s"
    c_cuddBddRestrict :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "wrappedRegular"
    c_wrappedRegular :: Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddPermute_s"
    c_cuddBddPermute :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Xgty_s"
	c_cuddXgty :: Ptr CDDManager -> CInt -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Inequality_s"
	c_cuddInequality :: Ptr CDDManager -> CInt -> CInt -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_Disequality_s"
	c_cuddDisequality :: Ptr CDDManager -> CInt -> CInt -> Ptr (Ptr CDDNode) -> Ptr (Ptr CDDNode) -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddInterval_s"
    c_cuddBddInterval :: Ptr CDDManager -> CInt -> Ptr (Ptr CDDNode) -> CInt -> CInt -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_NodeReadIndex"
    c_cuddNodeReadIndex :: Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_bddTransfer"
    c_cuddBddTransfer :: Ptr CDDManager -> Ptr CDDManager -> Ptr CDDNode -> IO (Ptr CDDNode)

foreign import ccall safe "&Cudd_RecursiveDeref"
	c_cuddRecursiveDerefPtr :: FunPtr (Ptr CDDManager -> Ptr CDDNode -> IO ())

foreign import ccall safe "&Cudd_DelayedDerefBdd"
	c_cuddDelayedDerefBddPtr :: FunPtr (Ptr CDDManager -> Ptr CDDNode -> IO ())

foreign import ccall safe "&Cudd_IterDerefBdd"
	c_cuddIterDerefBddPtr :: FunPtr (Ptr CDDManager -> Ptr CDDNode -> IO ())

foreign import ccall safe "Cudd_bddNewVar_s"
    c_cuddBddNewVar :: Ptr CDDManager -> IO (Ptr CDDNode)

foreign import ccall safe "Cudd_bddVectorCompose_s"
    c_cuddBddVectorCompose :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CDDNode) -> IO (Ptr CDDNode) 

foreign import ccall safe "Cudd_Quit"
    c_cuddQuit :: Ptr CDDManager -> IO ()

foreign import ccall safe "Cudd_PrintMinterm"
    c_cuddPrintMinterm :: Ptr CDDManager -> Ptr CDDNode -> IO ()

foreign import ccall safe "Cudd_CheckCube"
    c_cuddCheckCube :: Ptr CDDManager -> Ptr CDDNode -> IO CInt

foreign import ccall safe "Cudd_PrintInfo"
       c_cuddPrintInfo :: Ptr CDDManager -> Ptr CFile -> IO CInt

foreign import ccall safe "Cudd_PrintDebug"
    c_cuddPrintDebug :: Ptr CDDManager -> Ptr CDDNode -> CInt -> CInt -> IO CInt

foreign import ccall safe "wrappedCuddIsComplement"
    c_cuddIsComplement :: Ptr CDDNode -> CInt

foreign import ccall safe "Cudd_DumpDot"
    c_cuddDumpDot :: Ptr CDDManager -> CInt -> Ptr (Ptr CDDNode) -> Ptr CString -> Ptr CString -> Ptr CFile -> IO CInt

foreign import ccall safe "Cudd_FirstCube"
    c_cuddFirstCube :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CInt) -> Ptr CInt -> IO (Ptr CDDGen)

foreign import ccall safe "Cudd_NextCube"
    c_cuddNextCube :: Ptr CDDGen -> Ptr (Ptr CInt) -> Ptr CInt -> IO CInt

foreign import ccall safe "Cudd_FirstPrime"
    c_cuddFirstPrime :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr (Ptr CInt) -> IO (Ptr CDDGen)

foreign import ccall safe "Cudd_NextPrime"
    c_cuddNextPrime :: Ptr CDDGen -> Ptr (Ptr CInt) -> IO CInt

foreign import ccall safe "Cudd_IsGenEmpty"
    c_cuddIsGenEmpty :: Ptr CDDGen -> IO CInt

foreign import ccall safe "Cudd_GenFree"
    c_cuddGenFree :: Ptr CDDGen -> IO CInt

foreign import ccall safe "Cudd_FirstNode"
    c_cuddFirstNode :: Ptr CDDManager -> Ptr CDDNode -> Ptr (Ptr CDDNode) -> IO (Ptr CDDGen)

foreign import ccall safe "Cudd_NextNode"
    c_cuddNextNode :: Ptr CDDGen -> Ptr (Ptr CDDNode) -> IO CInt

