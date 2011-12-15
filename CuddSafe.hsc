{-# LANGUAGE RankNTypes #-}

module CuddSafe (DDPure, ManagerPure, DDContainer, runDDST, runDDSTNodes, runDDIO, purifyN, purifyM, purifyX, notb, andb, orb, xnorb, xorb, impb, iteb, existsb, forallb, getVar) where

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

purifyM :: STDdManager s -> DDContainer s t (ManagerPure t)
purifyM = DDContainer . ManagerPure . unSTDdManager

purifyN :: STDdNode s -> DDContainer s t (DDPure t)
purifyN = DDContainer . DDPure . unSTDdNode

purifyX :: s -> DDContainer s t t
purifyX _ = undefined

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

getVarBad :: Int -> DDPure t
getVarBad = undefined

getVarLessBad :: t -> Int -> DDPure t
getVarLessBad _ = undefined

testFunc :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> DDPure t
testFunc m x y z = orb m x (andb m y z)

testFunc2 :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> String
testFunc2 _ _ _ _ = "hello"

getS :: ST s s
getS = undefined

test = runST $ do
	s <- getS
	m <- cuddInitST
	cuddShuffleHeapST m [0..9]
	v1 <- cuddBddIthVarST m 0
	v2 <- cuddBddIthVarST m 1
	v3 <- cuddBddIthVarST m 2
	res1 <- liftM runIdentity $ runDDSTNodes (Identity <$> (testFunc <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3))
	res2 <- runDDST (testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3)
	res3 <- liftM runIdentity $ runDDSTNodes (Identity <$> ((getVar <$> purifyM m <*> pure 1)))
	res4 <- liftM runIdentity $ runDDSTNodes (Identity <$> (pure $ getVarBad 1))
	res5 <- runDDST (pure $ getVarBad 1)
	let a = getVarLessBad <$> purifyX s <*> pure 1
	let b = testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3
	--res6 <- runDDSTNodes undefined
	let c = getVar <$> purifyM m <*> pure 1 
	return ()

test2 = runST $ return $ newSTRef 5

f :: ManagerPure t -> DDPure t -> IO (String)
f = undefined

test3 = do
	m <- stToIO cuddInitST
	n <- stToIO $ cuddBddIthVarST m 0
	stToIO $ cuddShuffleHeapST m [0..9]
	res <- runDDIO $ f <$> purifyM m <*> purifyN n
	return res

