{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

module Cudd.File (
    DddmpVarInfoType(..),
    DddmpMode(..),
    DddmpVarMatchType(..),
    cuddBddStore,
    cuddBddLoad
    ) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Control.Monad

import Cudd.Cudd
import Cudd.C

#include "dddmp.h"

{#enum Dddmp_VarInfoType as DddmpVarInfoType {underscoreToCase} #}

--Hard coded values are needed because of limitations of c2hs
#c

enum DddmpMode {
    DddmpModeText    = 65,
    DddmpModeBinary  = 66,
    DddmpModeDefault = 68
};

#endc

{#enum DddmpMode {} #}

foreign import ccall safe "Dddmp_cuddBddStore"
    c_dddmpBddStore :: Ptr CDDManager -> CString -> Ptr CDDNode -> Ptr CString -> Ptr CInt -> CInt -> CInt -> CString -> Ptr CFile -> IO CInt

nullOnEmpty :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
nullOnEmpty lst act = 
    case lst of
        [] -> act nullPtr
        _  -> withArray lst act

cuddBddStore :: DDManager -> String -> DDNode -> [Int] -> DddmpMode -> DddmpVarInfoType -> String -> IO Bool
cuddBddStore (DDManager m) name (DDNode node) auxids mode varinfo fname = liftM (/= 0) $ 
    withCString name                      $ \pname ->
    withForeignPtr node                   $ \dp ->
    nullOnEmpty (map fromIntegral auxids) $ \pauxids -> 
    withCString fname                     $ \pfname -> 
        c_dddmpBddStore m pname dp nullPtr pauxids (fromIntegral $ fromEnum mode) (fromIntegral $ fromEnum varinfo) pfname nullPtr

{#enum Dddmp_VarMatchType as DddmpVarMatchType {underscoreToCase} #}

foreign import ccall safe "Dddmp_cuddBddLoad_s"
    c_dddmpBddLoad :: Ptr CDDManager -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt -> CString -> Ptr CFile -> IO (Ptr CDDNode)

cuddBddLoad :: DDManager -> DddmpVarMatchType -> [Int] -> [Int] -> DddmpMode -> String -> IO (Maybe DDNode)
cuddBddLoad (DDManager m) matchtype auxids composeids mode fname = 
    nullOnEmpty (map fromIntegral auxids)     $ \pauxids -> 
    nullOnEmpty (map fromIntegral composeids) $ \pcomposeids -> 
    withCString fname                         $ \pfname -> do
        node <- c_dddmpBddLoad m (fromIntegral $ fromEnum matchtype) nullPtr pauxids pcomposeids (fromIntegral $ fromEnum mode) pfname nullPtr
        case node == nullPtr of
            True  -> return Nothing
            False -> do
                fp <- newForeignPtrEnv deref m node
                return $ Just $ DDNode fp

