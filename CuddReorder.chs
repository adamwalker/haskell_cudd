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

import CuddInternal

#include <stdio.h>
#include <cudd.h>

{#enum Cudd_ReorderingType as CuddReorderingType {underscoreToCase} deriving (Show, Eq) #}

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

