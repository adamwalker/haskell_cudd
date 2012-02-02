{-# LANGUAGE RankNTypes #-}

module CuddSafe (
    DDPure, 
    ManagerPure, 
    DDContainer, 
    runDDST, 
    runDDSTNode,
    runDDSTNodes, 
    runDDIO, 
    purifyN, 
    purifyM, 
    notb, 
    andb, 
    orb, 
    xnorb, 
    xorb, 
    impb, 
    iteb, 
    existsb, 
    forallb, 
    getVar, 
    bb, 
    tb,
    swap,
    permute
    ) where

import Control.DeepSeq
import Control.Exception
import System.IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Control.Monad.ST.Lazy
import Data.STRef
import Control.Monad
import Control.Applicative
import Control.Monad.Identity

import CuddInternal
import Cudd

newtype DDPure t = DDPure {unDDPure :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)
instance NFData (DDPure t) 

newtype ManagerPure t = ManagerPure {unManagerPure :: Ptr CDdManager} deriving (Ord, Eq, Show)
instance NFData (ManagerPure t)

newtype DDContainer s t a = DDContainer {unDDContainer :: a}

instance Functor (DDContainer s t) where
	fmap f = DDContainer . f . unDDContainer

instance Applicative (DDContainer s t) where
	pure = DDContainer 
	(<*>) f = DDContainer . (unDDContainer f) . unDDContainer

runDDST :: (NFData a) => (forall t. DDContainer s t a) -> ST s a
runDDST x = unsafeIOToST $ evaluate $ force $ unDDContainer x

runDDIO :: (NFData a) => (forall t. DDContainer RealWorld t (IO a)) -> IO a
runDDIO (DDContainer x) = do
	res <- x
	evaluate (force res)

instance (NFData a) => NFData (Identity a) where
	rnf (Identity x) = rnf x

runDDSTNodes :: (Functor a, NFData (a (STDdNode s))) => (forall t. DDContainer s t (a (DDPure t))) -> ST s (a (STDdNode s))
runDDSTNodes (DDContainer x) = unsafeIOToST $ evaluate $ force $ fmap (STDdNode . unDDPure) x

runDDSTNode :: (forall t. DDContainer s t (DDPure t)) -> ST s (STDdNode s)
runDDSTNode (DDContainer x) = unsafeIOToST $ evaluate $ STDdNode $ unDDPure x

purifyM :: STDdManager s -> DDContainer s t (ManagerPure t)
purifyM = DDContainer . ManagerPure . unSTDdManager

purifyN :: STDdNode s -> DDContainer s t (DDPure t)
purifyN = DDContainer . DDPure . unSTDdNode

safeArg0 :: (DdManager -> DdNode) -> ManagerPure t -> DDPure t
safeArg0 f (ManagerPure m) = DDPure $ unDdNode $ f (DdManager m)

safeArg1 :: (DdManager -> DdNode -> DdNode) -> ManagerPure t -> DDPure t -> DDPure t
safeArg1 f (ManagerPure m) (DDPure x) = DDPure $ unDdNode $ f (DdManager m) (DdNode x)

safeArg2 :: (DdManager -> DdNode -> DdNode -> DdNode) -> ManagerPure t -> DDPure t -> DDPure t -> DDPure t
safeArg2 f (ManagerPure m) (DDPure x) (DDPure y) = DDPure $ unDdNode $ f (DdManager m) (DdNode x) (DdNode y)

safeArg3 :: (DdManager -> DdNode -> DdNode -> DdNode -> DdNode) -> ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> DDPure t
safeArg3 f (ManagerPure m) (DDPure x) (DDPure y) (DDPure z) = DDPure $ unDdNode $ f (DdManager m) (DdNode x) (DdNode y) (DdNode z)

bb = safeArg0 cuddReadLogicZero
tb = safeArg0 cuddReadOne

notb :: ManagerPure t -> DDPure t -> DDPure t
notb = safeArg1 cuddNot

andb, orb, xnorb, xorb, impb :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t
andb  = safeArg2 cuddBddAnd
orb   = safeArg2 cuddBddOr
xnorb = safeArg2 cuddBddXnor
xorb  = safeArg2 cuddBddXor
impb  = safeArg2 cuddBddImp

iteb :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> DDPure t
iteb  = safeArg3 cuddBddIte

existsb, forallb :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t
existsb = safeArg2 cuddBddExistAbstract
forallb = safeArg2 cuddBddUnivAbstract

getVar :: ManagerPure t -> Int -> DDPure t
getVar (ManagerPure m) i = DDPure $ unDdNode $ cuddBddIthVar (DdManager m) i

permute :: ManagerPure t -> DDPure t -> [Int] -> DDPure t
permute (ManagerPure m) (DDPure d) perm = DDPure $ unDdNode $ cuddBddPermute (DdManager m) (DdNode d) perm

swap :: ManagerPure t -> DDPure t -> [DDPure t] -> [DDPure t] -> DDPure t
swap (ManagerPure m) (DDPure d) x y = DDPure $ unDdNode $ cuddBddSwapVariables (DdManager m) (DdNode d) (map (DdNode . unDDPure) x) (map (DdNode . unDDPure) y) 

