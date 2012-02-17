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
    purifyNT,
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
    permute,
    varMap,
    andAbs,
    xorAbs,
    transfer,
    liCompaction,
    minimize
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
import Data.Traversable

import CuddInternal
import Cudd

newtype DDPure t u = DDPure {unDDPure :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)
instance NFData (DDPure t u) 

newtype ManagerPure t u = ManagerPure {unManagerPure :: Ptr CDdManager} deriving (Ord, Eq, Show)
instance NFData (ManagerPure t u)

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

runDDSTNodes :: (Functor a, NFData (a (STDdNode s u))) => (forall t. DDContainer s t (a (DDPure t u))) -> ST s (a (STDdNode s u))
runDDSTNodes (DDContainer x) = unsafeIOToST $ evaluate $ force $ fmap (STDdNode . unDDPure) x

runDDSTNode :: (forall t. DDContainer s t (DDPure t u)) -> ST s (STDdNode s u)
runDDSTNode (DDContainer x) = unsafeIOToST $ evaluate $ STDdNode $ unDDPure x

purifyM :: STDdManager s u -> DDContainer s t (ManagerPure t u)
purifyM = DDContainer . ManagerPure . unSTDdManager

purifyN :: STDdNode s u -> DDContainer s t (DDPure t u)
purifyN = DDContainer . DDPure . unSTDdNode

purifyNT :: (Traversable f) => f (STDdNode s u) -> DDContainer s t (f (DDPure t u))
purifyNT = traverse purifyN

safeArg0 :: (DdManager -> DdNode) -> ManagerPure t u -> DDPure t u
safeArg0 f (ManagerPure m) = DDPure $ unDdNode $ f (DdManager m)

safeArg1 :: (DdManager -> DdNode -> DdNode) -> ManagerPure t u -> DDPure t u -> DDPure t u
safeArg1 f (ManagerPure m) (DDPure x) = DDPure $ unDdNode $ f (DdManager m) (DdNode x)

safeArg2 :: (DdManager -> DdNode -> DdNode -> DdNode) -> ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u
safeArg2 f (ManagerPure m) (DDPure x) (DDPure y) = DDPure $ unDdNode $ f (DdManager m) (DdNode x) (DdNode y)

safeArg3 :: (DdManager -> DdNode -> DdNode -> DdNode -> DdNode) -> ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u -> DDPure t u
safeArg3 f (ManagerPure m) (DDPure x) (DDPure y) (DDPure z) = DDPure $ unDdNode $ f (DdManager m) (DdNode x) (DdNode y) (DdNode z)

bb = safeArg0 cuddReadLogicZero
tb = safeArg0 cuddReadOne

notb :: ManagerPure t u -> DDPure t u -> DDPure t u
notb = safeArg1 cuddNot

andb, orb, xnorb, xorb, impb :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u
andb  = safeArg2 cuddBddAnd
orb   = safeArg2 cuddBddOr
xnorb = safeArg2 cuddBddXnor
xorb  = safeArg2 cuddBddXor
impb  = safeArg2 cuddBddImp

iteb :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u -> DDPure t u
iteb  = safeArg3 cuddBddIte

existsb, forallb :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u
existsb = safeArg2 cuddBddExistAbstract
forallb = safeArg2 cuddBddUnivAbstract

getVar :: ManagerPure t u -> Int -> DDPure t u
getVar (ManagerPure m) i = DDPure $ unDdNode $ cuddBddIthVar (DdManager m) i

permute :: ManagerPure t u -> DDPure t u -> [Int] -> DDPure t u
permute (ManagerPure m) (DDPure d) perm = DDPure $ unDdNode $ cuddBddPermute (DdManager m) (DdNode d) perm

swap :: ManagerPure t u -> DDPure t u -> [DDPure t u] -> [DDPure t u] -> DDPure t u
swap (ManagerPure m) (DDPure d) x y = DDPure $ unDdNode $ cuddBddSwapVariables (DdManager m) (DdNode d) (map (DdNode . unDDPure) x) (map (DdNode . unDDPure) y) 

varMap :: ManagerPure t u -> DDPure t u -> DDPure t u
varMap (ManagerPure m) (DDPure d) = DDPure $ unSTDdNode $ unsafePerformIO $ stToIO $ cuddBddVarMapST (STDdManager m) (STDdNode d)

andAbs, xorAbs :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u -> DDPure t u
andAbs = safeArg3 cuddBddAndAbstract
xorAbs = safeArg3 cuddBddXorExistAbstract

transfer :: ManagerPure t u -> ManagerPure t v -> DDPure t u -> DDPure t v
transfer (ManagerPure m1) (ManagerPure m2) (DDPure d1) = DDPure $ unDdNode $ cuddBddTransfer (DdManager m1) (DdManager m2) (DdNode d1)

liCompaction, minimize :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u
liCompaction = safeArg2 cuddBddLICompaction
minimize     = safeArg2 cuddBddMinimize

