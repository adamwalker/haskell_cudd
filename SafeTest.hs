module SafeTest where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.DeepSeq
import Control.Monad.ST.Lazy

import Cudd
import CuddSafe

testFunc :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> DDPure t
testFunc m x y z = orb m x (andb m y z)

testFunc2 :: ManagerPure t -> DDPure t -> DDPure t -> DDPure t -> String
testFunc2 _ _ _ _ = "hello"

test = runST $ do
	m <- cuddInitST
	cuddShuffleHeapST m [0..9]
	v1 <- cuddBddIthVarST m 0
	v2 <- cuddBddIthVarST m 1
	v3 <- cuddBddIthVarST m 2
	res1 <- liftM runIdentity $ runDDSTNodes (Identity <$> (testFunc <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3))
	res2 <- runDDST (testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3)
	res3 <- liftM runIdentity $ runDDSTNodes (Identity <$> ((getVar <$> purifyM m <*> pure 1)))
	let b = testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3
	--res6 <- runDDSTNodes undefined
	let c = getVar <$> purifyM m <*> pure 1 
	let x = getVar undefined 5 == getVar undefined 6
	when x (error "asdf")
	return x

test2 = runST $ do
	m <- cuddInitST
	cuddShuffleHeapST m [0..9]
	v1 <- cuddBddIthVarST m 0
	v2 <- cuddBddIthVarST m 1
	v3 <- cuddBddIthVarST m 2
	res1 <- liftM runIdentity $ runDDSTNodes (Identity <$> (testFunc <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3))
	res2 <- runDDST (testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3)
	res3 <- liftM runIdentity $ runDDSTNodes (Identity <$> ((getVar <$> purifyM m <*> pure 1)))
	let b = testFunc2 <$> purifyM m <*> purifyN v1 <*> purifyN v2 <*> purifyN v3
	--res6 <- runDDSTNodes undefined
	let c = getVar <$> purifyM m <*> pure 1 
	return ()

f :: ManagerPure t -> DDPure t -> IO (String)
f = undefined

test3 = do
	m <- stToIO cuddInitST
	n <- stToIO $ cuddBddIthVarST m 0
	stToIO $ cuddShuffleHeapST m [0..9]
	res <- runDDIO $ f <$> purifyM m <*> purifyN n
	return res

