{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module CuddInternal (CDdManager(..), DdManager(..), STDdManager(..), STDdNode(..), CDdNode(..), DdNode(..), c_cuddRecursiveDeref, cuddRef, withForeignArray, withForeignArrayPtr, withForeignArrayPtrLen, ddNodeToInt) where

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

newtype STDdManager s = STDdManager {unSTDdManager :: Ptr CDdManager}

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

newtype STDdNode s = STDdNode {unSTDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

instance NFData (STDdNode s)

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode 

foreign import ccall unsafe "cudd.h &Cudd_RecursiveDeref"
	c_cuddRecursiveDeref :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall unsafe "cuddwrap.h wrappedCuddRef"
	cuddRef :: Ptr CDdNode -> IO ()

withForeignArray :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignArray [] func = func []
withForeignArray (p:ptrs) func = withForeignPtr p $ \ptr -> 
    withForeignArray ptrs $ \x -> func (ptr:x)

withForeignArrayPtr :: [ForeignPtr a] -> (Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtr fps func = withForeignArray fps $ \ap -> withArray ap func

withForeignArrayPtrLen :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtrLen fps func = withForeignArray fps $ \ap -> withArrayLen ap func
