{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}

module CuddC (
    CDdManager,
    CDdNode,
    c_cuddReadOne,
    c_cuddReadLogicZero,
    c_cuddBddIthVar,
    c_cuddBddAnd,
    c_cuddBddOr,
    c_cuddBddNand,
    c_cuddBddNor,
    c_cuddBddXor,
    c_cuddBddXnor,
    c_cuddNot,
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
    c_cuddEquivDC
    ) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types

data CDdManager
data CDdNode = CDdNode {index :: CInt, ref :: CInt}

{-
instance Storable CDdNode where
	sizeOf _ = (#size DdNode)
	alignment _ = alignment (undefined :: Int)
	peek ptr = do
		index <- (#peek DdNode, index) ptr
		ref <- (#peek DdNode, ref) ptr
		return $ CDdNode index ref
	poke ptr (CDdNode index ref) = do
		(#poke DdNode, index) ptr index
		(#poke DdNode, ref) ptr ref
        -}

foreign import ccall safe "cudd.h Cudd_ReadOne_s"
	c_cuddReadOne :: Ptr CDdManager -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_ReadLogicZero_s"
	c_cuddReadLogicZero :: Ptr CDdManager -> IO (Ptr CDdNode)

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

foreign import ccall safe "cuddwrap.h wrappedCuddNot_s"
	c_cuddNot :: Ptr CDdNode -> IO (Ptr CDdNode)

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

foreign import ccall safe "cudd.h Cudd_SupportIndex"
	c_cuddSupportIndex :: Ptr CDdManager -> Ptr CDdNode -> IO(Ptr CInt)

foreign import ccall safe "cudd.h Cudd_SupportIndices"
    c_cuddSupportIndices :: Ptr CDdManager -> Ptr CDdNode -> Ptr (Ptr CInt) -> IO (CInt)

foreign import ccall safe "cudd.h Cudd_IndicesToCube_s"
    c_cuddIndicesToCube :: Ptr CDdManager -> Ptr CInt -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_bddComputeCube_s"
    c_cuddBddComputeCube :: Ptr CDdManager -> Ptr (Ptr CDdNode) -> Ptr CInt -> CInt -> IO (Ptr CDdNode)

foreign import ccall safe "cudd.h Cudd_BddToCubeArray"
    c_cuddBddToCubeArray :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (CInt)

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

