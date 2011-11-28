{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}

module MTR where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad

#include <stdio.h>
#include <mtr.h>

data CMtrNode 
newtype MtrNode = MtrNode (Ptr CMtrNode)

foreign import ccall unsafe "mtr.h Mtr_AllocNode"
    c_mtrAllocNode :: IO (Ptr CMtrNode)

mtrAllocNode :: IO (MtrNode) 
mtrAllocNode = liftM MtrNode c_mtrAllocNode

foreign import ccall unsafe "mtr.h Mtr_CreateFirstChild"
    c_mtrCreateFirstChild :: Ptr CMtrNode -> IO (Ptr CMtrNode)

mtrCreateFirstChild :: MtrNode -> IO (MtrNode)
mtrCreateFirstChild (MtrNode p) = liftM MtrNode $ c_mtrCreateFirstChild p

foreign import ccall unsafe "mtr.h Mtr_CreateLastChild"
    c_mtrCreateLastChild :: Ptr CMtrNode -> IO (Ptr CMtrNode)

mtrCreateLastChild :: MtrNode -> IO (MtrNode)
mtrCreateLastChild (MtrNode p) = liftM MtrNode $ c_mtrCreateLastChild p

foreign import ccall unsafe "mtr.h Mtr_DeallocNode"
    c_mtrDeallocNode :: Ptr CMtrNode -> IO ()

mtrDeallocNode :: MtrNode -> IO ()
mtrDeallocNode (MtrNode p) = c_mtrDeallocNode p

foreign import ccall unsafe "mtr.h Mtr_MakeFirstChild"
    c_mtrMakeFirstChild :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeFirstChild :: MtrNode -> MtrNode -> IO ()
mtrMakeFirstChild (MtrNode p) (MtrNode c) = c_mtrMakeFirstChild p c

foreign import ccall unsafe "mtr.h Mtr_MakeLastChild"
    c_mtrMakeLastChild :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeLastChild :: MtrNode -> MtrNode -> IO ()
mtrMakeLastChild (MtrNode p) (MtrNode c) = c_mtrMakeLastChild p c

foreign import ccall unsafe "mtr.h Mtr_MakeNextSibling"
    c_mtrMakeNextSibling :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeNextSibling :: MtrNode -> MtrNode -> IO ()
mtrMakeNextSibling (MtrNode f) (MtrNode s) = c_mtrMakeNextSibling f s

foreign import ccall unsafe "mtr.h Mtr_PrintGroups"
    c_mtrPrintGroups :: Ptr CMtrNode -> CInt -> IO ()

mtrPrintGroups :: MtrNode -> Int -> IO ()
mtrPrintGroups (MtrNode c) s = c_mtrPrintGroups c (fromIntegral s)

foreign import ccall unsafe "mtr.h Mtr_PrintTree"
    c_mtrPrintTree :: Ptr CMtrNode -> IO ()

mtrPrintTree :: MtrNode -> IO ()
mtrPrintTree (MtrNode c) = c_mtrPrintTree c

foreign import ccall unsafe "mtr.h Mtr_MakeGroup"
    c_mtrMakeGroup :: Ptr CMtrNode -> CInt -> CInt -> CInt -> IO (Ptr CMtrNode)

mtrMakeGroup :: MtrNode -> Int -> Int -> Int -> IO (MtrNode)
mtrMakeGroup (MtrNode r) l s f = liftM MtrNode $ c_mtrMakeGroup r (fromIntegral l) (fromIntegral s) (fromIntegral f)

foreign import ccall unsafe "mtr.h Mtr_InitGroupTree"
    c_mtrInitGroupTree :: CInt -> CInt -> IO (Ptr CMtrNode)

mtrInitGroupTree :: Int -> Int -> IO (MtrNode)
mtrInitGroupTree l s = liftM MtrNode $ c_mtrInitGroupTree (fromIntegral l) (fromIntegral s)

#c
enum MTR_TYPES {
    MTRDefault = MTR_DEFAULT,
    MTRTerminal = MTR_TERMINAL,
    MTRSoft = MTR_SOFT,
    MTRFixed = MTR_FIXED,
    MTRNewNode = MTR_NEWNODE
};
#endc

{#enum MTR_TYPES as MTRType {underscoreToCase} deriving (Show, Eq) #}

