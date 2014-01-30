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

import ForeignHelpers
import CuddInternal
import MTR
import CuddC
import CuddCommon

#include "dddmp.h"

newtype Dddmp_VarInfoType = Dddmp_VarInfoType {varInfoTypeVal :: CInt}
#{enum Dddmp_VarInfoType, Dddmp_VarInfoType
  , dddmp_varids      = DDDMP_VARIDS
  , dddmp_varpermids  = DDDMP_VARPERMIDS
  , dddmp_varauxids   = DDDMP_VARAUXIDS
  , dddmp_varnames    = DDDMP_VARNAMES
  , dddmp_vardefault  = DDDMP_VARDEFAULT
  }

newtype Dddmp_Mode = Dddmp_Mode {dddmpMode :: CInt}
#{enum Dddmp_Mode, Dddmp_Mode
  , dddmp_mode_text    = DDDMP_MODE_TEXT
  , dddmp_mode_binary  = DDDMP_MODE_BINARY
  , dddmp_mode_default = DDDMP_MODE_DEFAULT
  }


newtype Dddmp_Status = Dddmp_Status {dddmpStatus :: CInt} deriving (Eq, Show)
#{enum Dddmp_Status, Dddmp_Status
  , dddmp_failure    = DDDMP_FAILURE
  , dddmp_success    = DDDMP_SUCCESS
  } 


foreign import ccall safe "dddmp.h Dddmp_cuddBddStore"
    c_dddmpBddStore :: Ptr CDdManager -> CString -> Ptr CDdNode -> Ptr CString -> Ptr CInt -> CInt -> CInt -> CString -> Ptr CFile -> IO CInt

cuddBddStore :: DdManager -> String -> DdNode -> [Int] -> Dddmp_Mode -> Dddmp_VarInfoType -> String -> IO Dddmp_Status
cuddBddStore (DdManager m) name (DdNode node) auxids mode varinfo fname = do
    pauxids <- case auxids of
                [] -> return nullPtr
                _ -> newArray (map fromIntegral auxids :: [CInt])
    withForeignPtr node $ \dp -> do 
    withCString name $ \pname -> do
    withCString fname $ \pfname -> do
        ret <- c_dddmpBddStore m pname dp nullPtr pauxids (dddmpMode mode) (varInfoTypeVal varinfo) pfname nullPtr
        return $ Dddmp_Status ret

-- Extremely ugly and unsafe way to convert BDD to String via file
bddToString :: (MonadError String me) => DdManager -> DdNode -> me String
bddToString m node = unsafePerformIO $ 
    catchError (do let fname = show (unDdNode node) ++ ".bdd"
                   ret <- cuddBddStore m fname node [] dddmp_mode_text dddmp_varids fname
                   --putStrLn $ "ret = " ++ (show ret)
                   if ret == dddmp_success
                           then do str <- readFile fname
                                   removeFile fname
                                   return $ return str
                           else return $ throwError $ "Failed to serialise BDD (status: " ++ show (dddmpStatus ret) ++ ")")
               (return . throwError . show)
    

newtype Dddmp_VarMatchType = Dddmp_VarMatchType {dddmpMatchType :: CInt} deriving (Eq, Show)
#{enum Dddmp_VarMatchType, Dddmp_VarMatchType
  , dddmp_var_matchids     = DDDMP_VAR_MATCHIDS
  , dddmp_var_matchpermids = DDDMP_VAR_MATCHPERMIDS
  , dddmp_var_matchauxids  = DDDMP_VAR_MATCHAUXIDS
  , dddmp_var_matchnames   = DDDMP_VAR_MATCHNAMES
  , dddmp_var_composeids   = DDDMP_VAR_COMPOSEIDS
  } 

foreign import ccall safe "dddmp.h Dddmp_cuddBddLoad_s"
    c_dddmpBddLoad :: Ptr CDdManager -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt -> CString -> Ptr CFile -> IO (Ptr CDdNode)

cuddBddLoad :: DdManager -> Dddmp_VarMatchType -> [Int] -> [Int] -> Dddmp_Mode -> String -> IO DdNode
cuddBddLoad (DdManager m) matchtype auxids composeids mode fname = do
    pauxids <- case auxids of
                 [] -> return nullPtr
                 _ -> newArray (map fromIntegral auxids :: [CInt])
    pcomposeids <- case auxids of
                     [] -> return nullPtr
                     _ -> newArray (map fromIntegral composeids :: [CInt])
    withCString fname $ \pfname -> do
        node <- c_dddmpBddLoad m (dddmpMatchType matchtype) nullPtr pauxids pcomposeids (dddmpMode mode) pfname nullPtr
        if node == nullPtr
            then ioError $ userError "Dddmp_cuddBddLoad failed"
            else do 
                    fp <- newForeignPtrEnv deref m node
                    return $ DdNode fp

-- BDD from string via file
bddFromString :: MonadError String me => DdManager -> String -> me DdNode
bddFromString m str = unsafePerformIO $ 
    catchError (do let fname = "_fromString.bdd"
                   writeFile fname str
                   node <- cuddBddLoad m dddmp_var_matchids [] [] dddmp_mode_text fname
                   removeFile fname
                   return $ return node)
               (return . throwError . show)
