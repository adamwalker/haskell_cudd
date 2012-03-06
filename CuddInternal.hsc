{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module CuddInternal (
    CDdManager(..), 
    DdManager(..), 
    STDdManager(..), 
    STDdNode(..), 
    CDdNode(..), 
    DdNode(..), 
    c_cuddRecursiveDeref, 
    c_cuddDelayedDerefBdd,
    c_cuddIterDerefBdd,
    cuddRef, 
    withForeignArray, 
    withForeignArrayPtr, 
    withForeignArrayPtrLen, 
    ddNodeToInt,
    deref
    ) where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Control.Monad.ST.Lazy
import Control.Monad
import Control.DeepSeq

#include "cudd.h"
#include "cuddwrap.h"

data CDdManager
newtype DdManager = DdManager (Ptr CDdManager)

newtype STDdManager s u = STDdManager {unSTDdManager :: Ptr CDdManager}

data CDdNode = CDdNode {index :: CInt, ref :: CInt}
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

newtype DdNode = DdNode {unDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

newtype STDdNode s u = STDdNode {unSTDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

instance NFData (STDdNode s u)

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode 

foreign import ccall safe "cudd.h &Cudd_RecursiveDeref"
	c_cuddRecursiveDeref :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_DelayedDerefBdd"
	c_cuddDelayedDerefBdd :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cudd.h &Cudd_IterDerefBdd"
	c_cuddIterDerefBdd :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall safe "cuddwrap.h wrappedCuddRef"
	cuddRef :: Ptr CDdNode -> IO ()

deref = c_cuddIterDerefBdd

withForeignArray :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignArray [] func = func []
withForeignArray (p:ptrs) func = withForeignPtr p $ \ptr -> 
    withForeignArray ptrs $ \x -> func (ptr:x)

withForeignArrayPtr :: [ForeignPtr a] -> (Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtr fps func = withForeignArray fps $ \ap -> withArray ap func

withForeignArrayPtrLen :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtrLen fps func = withForeignArray fps $ \ap -> withArrayLen ap func
