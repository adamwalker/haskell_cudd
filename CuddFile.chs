{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

module CuddFile where

import System.IO
import System.Directory
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe
import Control.Monad.ST
import Control.Monad
import Data.Binary
import Data.List
import Control.DeepSeq
import Control.Monad.Error
import Data.Array hiding (indices)
import Control.Exception
import Data.Maybe

import ForeignHelpers
import CuddInternal
import MTR
import CuddC
import CuddCommon

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

foreign import ccall safe "dddmp.h Dddmp_cuddBddStore"
    c_dddmpBddStore :: Ptr CDdManager -> CString -> Ptr CDdNode -> Ptr CString -> Ptr CInt -> CInt -> CInt -> CString -> Ptr CFile -> IO CInt

nullOnEmpty :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
nullOnEmpty lst act = 
    case lst of
        [] -> act nullPtr
        _  -> withArray lst act

cuddBddStore :: DdManager -> String -> DdNode -> [Int] -> DddmpMode -> DddmpVarInfoType -> String -> IO Bool
cuddBddStore (DdManager m) name (DdNode node) auxids mode varinfo fname = liftM (/= 0) $ 
    withCString name                      $ \pname ->
    withForeignPtr node                   $ \dp ->
    nullOnEmpty (map fromIntegral auxids) $ \pauxids -> 
    withCString fname                     $ \pfname -> 
        c_dddmpBddStore m pname dp nullPtr pauxids (fromIntegral $ fromEnum mode) (fromIntegral $ fromEnum varinfo) pfname nullPtr

{#enum Dddmp_VarMatchType as DddmpVarMatchType {underscoreToCase} #}

foreign import ccall safe "dddmp.h Dddmp_cuddBddLoad_s"
    c_dddmpBddLoad :: Ptr CDdManager -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt -> CString -> Ptr CFile -> IO (Ptr CDdNode)

cuddBddLoad :: DdManager -> DddmpVarMatchType -> [Int] -> [Int] -> DddmpMode -> String -> IO (Maybe DdNode)
cuddBddLoad (DdManager m) matchtype auxids composeids mode fname = 
    nullOnEmpty (map fromIntegral auxids)     $ \pauxids -> 
    nullOnEmpty (map fromIntegral composeids) $ \pcomposeids -> 
    withCString fname                         $ \pfname -> do
        node <- c_dddmpBddLoad m (fromIntegral $ fromEnum matchtype) nullPtr pauxids pcomposeids (fromIntegral $ fromEnum mode) pfname nullPtr
        case node == nullPtr of
            True  -> return Nothing
            False -> do
                fp <- newForeignPtrEnv deref m node
                return $ Just $ DdNode fp

-- BDD from string via file
bddFromString :: MonadError String me => DdManager -> String -> me DdNode
bddFromString m str = unsafePerformIO $ 
    catchError (do let fname = "_fromString.bdd"
                   writeFile fname str
                   node <- cuddBddLoad m DddmpVarMatchauxids [] [] DddmpModeText fname
                   removeFile fname
                   return $ return $ fromJust node)
               (return . throwError . show)

-- Extremely ugly and unsafe way to convert BDD to String via file
bddToString :: (MonadError String me) => DdManager -> DdNode -> me String
bddToString m node = unsafePerformIO $ 
    catchError (do let fname = show (unDdNode node) ++ ".bdd"
                   ret <- cuddBddStore m fname node [] DddmpModeText DddmpVarids fname
                   --putStrLn $ "ret = " ++ (show ret)
                   if ret == True
                           then do str <- readFile fname
                                   removeFile fname
                                   return $ return str
                           else return $ throwError $ "Failed to serialise BDD (status: " ++ show ret ++ ")")
               (return . throwError . show)
