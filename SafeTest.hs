module SafeTest where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.DeepSeq
import Control.Monad.ST

import Cudd
import CuddSafe

testFunc :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u -> DDPure t u
testFunc m x y z = orb m x (andb m y z)

testFunc2 :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u -> String
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

f :: ManagerPure t u -> DDPure t u -> IO (String)
f = undefined

test3 = do
	m <- stToIO cuddInitST
	n <- stToIO $ cuddBddIthVarST m 0
	stToIO $ cuddShuffleHeapST m [0..9]
	res <- runDDIO $ f <$> purifyM m <*> purifyN n
	return res

func :: STDdManager s u -> STDdNode s u -> STDdNode s u -> ST s (STDdNode s u)
func = undefined

{-
test4 = do
    withManagerST $ \m -> do
        withManagerST $ \m' -> do
            v1 <- cuddBddIthVarST m 0 
            v2 <- cuddBddIthVarST m' 1
            res <- func m v1 v2
            return $ res == undefined
-}

func2 :: ManagerPure t u -> DDPure t u -> DDPure t u -> DDPure t u
func2 = undefined

{-
test5 = do
    withManagerST $ \m -> do
        withManagerST $ \m' -> do
            v1 <- cuddBddIthVarST m 0 
            v2 <- cuddBddIthVarST m' 1
            res <- runDDSTNode $ func2 <$> purifyM m <*> purifyN v1 <*> purifyN v2
            return $ res == undefined
-}

func3 :: ManagerPure t u -> ManagerPure t v -> DDPure t u -> DDPure t v -> DDPure t u
func3 m1 m2 n1 n2 = func2 m1 n1 (transfer m2 m1 n2)

test5 = do
    withManagerST $ \m -> do
        withManagerST $ \m' -> do
            v1 <- cuddBddIthVarST m 0 
            v2 <- cuddBddIthVarST m' 1
            res <- runDDSTNode $ func3 <$> purifyM m <*> purifyM m' <*> purifyN v1 <*> purifyN v2
            return $ res == undefined

