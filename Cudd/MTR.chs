{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}

module Cudd.MTR (
    MtrNode(..),
    CMtrNode,
    mtrAllocNode,
    mtrCreateFirstChild,
    mtrCreateLastChild,
    mtrDeallocNode,
    mtrMakeFirstChild,
    mtrMakeLastChild,
    mtrMakeNextSibling,
    mtrPrintGroups,
    mtrPrintTree,
    mtrMakeGroup,
    mtrInitGroupTree,
    mtrFindGroup,
    mtrDissolveGroup,
    MTRType(..)
    ) where

import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

#include <stdio.h>
#include <mtr.h>

data CMtrNode 
newtype MtrNode s = MtrNode (Ptr CMtrNode)

foreign import ccall unsafe "Mtr_AllocNode"
    c_mtrAllocNode :: IO (Ptr CMtrNode)

mtrAllocNode :: ST s (MtrNode s) 
mtrAllocNode = liftM MtrNode $ unsafeIOToST c_mtrAllocNode

foreign import ccall unsafe "Mtr_CreateFirstChild"
    c_mtrCreateFirstChild :: Ptr CMtrNode -> IO (Ptr CMtrNode)

mtrCreateFirstChild :: MtrNode s -> ST s (MtrNode s)
mtrCreateFirstChild (MtrNode p) = liftM MtrNode $ unsafeIOToST $ c_mtrCreateFirstChild p

foreign import ccall unsafe "Mtr_CreateLastChild"
    c_mtrCreateLastChild :: Ptr CMtrNode -> IO (Ptr CMtrNode)

mtrCreateLastChild :: MtrNode s -> ST s (MtrNode s)
mtrCreateLastChild (MtrNode p) = liftM MtrNode $ unsafeIOToST $ c_mtrCreateLastChild p

foreign import ccall unsafe "Mtr_DeallocNode"
    c_mtrDeallocNode :: Ptr CMtrNode -> IO ()

mtrDeallocNode :: MtrNode s -> ST s ()
mtrDeallocNode (MtrNode p) = unsafeIOToST $ c_mtrDeallocNode p

foreign import ccall unsafe "Mtr_MakeFirstChild"
    c_mtrMakeFirstChild :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeFirstChild :: MtrNode s -> MtrNode s -> ST s ()
mtrMakeFirstChild (MtrNode p) (MtrNode c) = unsafeIOToST $ c_mtrMakeFirstChild p c

foreign import ccall unsafe "Mtr_MakeLastChild"
    c_mtrMakeLastChild :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeLastChild :: MtrNode s -> MtrNode s -> ST s ()
mtrMakeLastChild (MtrNode p) (MtrNode c) = unsafeIOToST $ c_mtrMakeLastChild p c

foreign import ccall unsafe "Mtr_MakeNextSibling"
    c_mtrMakeNextSibling :: Ptr CMtrNode -> Ptr CMtrNode -> IO ()

mtrMakeNextSibling :: MtrNode s -> MtrNode s -> ST s ()
mtrMakeNextSibling (MtrNode f) (MtrNode s) = unsafeIOToST $ c_mtrMakeNextSibling f s

foreign import ccall unsafe "Mtr_PrintGroups"
    c_mtrPrintGroups :: Ptr CMtrNode -> CInt -> IO ()

mtrPrintGroups :: MtrNode s -> Int -> ST s ()
mtrPrintGroups (MtrNode c) s = unsafeIOToST $ c_mtrPrintGroups c (fromIntegral s)

foreign import ccall unsafe "Mtr_PrintTree"
    c_mtrPrintTree :: Ptr CMtrNode -> IO ()

mtrPrintTree :: MtrNode s -> ST s ()
mtrPrintTree (MtrNode c) = unsafeIOToST $ c_mtrPrintTree c

foreign import ccall unsafe "Mtr_MakeGroup"
    c_mtrMakeGroup :: Ptr CMtrNode -> CInt -> CInt -> CInt -> IO (Ptr CMtrNode)

mtrMakeGroup :: MtrNode s -> Int -> Int -> Int -> ST s (MtrNode s)
mtrMakeGroup (MtrNode r) l s f = liftM MtrNode $ unsafeIOToST $ c_mtrMakeGroup r (fromIntegral l) (fromIntegral s) (fromIntegral f)

foreign import ccall unsafe "Mtr_InitGroupTree"
    c_mtrInitGroupTree :: CInt -> CInt -> IO (Ptr CMtrNode)

mtrInitGroupTree :: Int -> Int -> ST s (MtrNode s)
mtrInitGroupTree l s = liftM MtrNode $ unsafeIOToST $ c_mtrInitGroupTree (fromIntegral l) (fromIntegral s)

foreign import ccall unsafe "Mtr_FindGroup"
    c_mtrFindGroup :: Ptr CMtrNode -> CUInt -> CUInt -> IO (Ptr CMtrNode)

mtrFindGroup :: MtrNode s -> Int -> Int -> ST s (MtrNode s)
mtrFindGroup (MtrNode m) x y = liftM MtrNode $ unsafeIOToST $ c_mtrFindGroup m (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "Mtr_DissolveGroup"
    c_mtrDissolveGroup :: Ptr CMtrNode -> IO ()

mtrDissolveGroup :: MtrNode s -> ST s ()
mtrDissolveGroup (MtrNode m) = unsafeIOToST $ c_mtrDissolveGroup m

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

