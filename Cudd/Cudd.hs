{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

module Cudd.Cudd (
    DdManager(),
    DdNode(),
    cuddInit,
    cuddInitOrder,
    cuddReadOne,
    cuddReadLogicZero,
    cuddBddIthVar,
    cuddBddAnd,
    cuddBddOr,
    cuddBddNand,
    cuddBddNor,
    cuddBddXor,
    cuddBddXnor,
    cuddNot,
    cuddDumpDot,
    cudd_cache_slots,
    cudd_unique_slots,
    cuddEval,
    cuddPrintMinterm,
    cuddAllSat,
    cuddOneSat,
    cuddOnePrime,
    cuddSupportIndex,
    cuddBddExistAbstract,
    cuddBddUnivAbstract,
    cuddBddIte,
    cuddBddPermute,
    cuddBddShift,
    cuddBddSwapVariables,
    cuddNodeReadIndex,
    cuddDagSize,
    cuddIndicesToCube,
    getManagerST,
    getSTManager,
    cuddBddLICompaction,
    cuddBddMinimize,
    cuddReadSize,
    cuddXeqy,
    cuddXgty,
    cuddBddInterval,
    cuddDisequality,
    cuddInequality,
    ddNodeToInt,
    cuddBddImp,
    cuddBddPickOneMinterm,
    cuddReadPerm,
    cuddReadInvPerm,
    cuddReadPerms,
    cuddReadInvPerms,
    cuddReadTree,
    cuddCountLeaves,
    cuddCountMinterm,
    cuddCountPath,
    cuddCountPathsToNonZero,
    cuddPrintDebug,
    cuddBddAndAbstract,
    cuddBddXorExistAbstract,
    cuddBddTransfer,
    cuddBddMakePrime,
    cuddBddConstrain,
    cuddBddRestrict,
    cuddBddSqueeze,
    SatBit(..),
    cuddLargestCube,
    cuddBddLeq,
    cuddDebugCheck,
    cuddCheckKeys,
    cuddPrintInfo
    ) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad
import Data.List
import Data.Array hiding (indices)
import Control.Exception

import Cudd.ForeignHelpers
import Cudd.Internal
import Cudd.MTR
import Cudd.C
import Cudd.Common

cuddInit :: DdManager
cuddInit = DdManager $ unsafePerformIO $ c_cuddInit 0 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0

cuddInitOrder :: [Int] -> DdManager
cuddInitOrder order = DdManager $ unsafePerformIO $ withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "cuddInitOrder: order does not contain each variable once") 
    m <- c_cuddInit (fromIntegral size) 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0
    res <- c_cuddShuffleHeap m ptr
    when (res /= 1) (error "shuffleHeap failed")
    return m

getManagerST :: STDdManager s u -> DdManager
getManagerST (STDdManager m) = DdManager m

getSTManager :: DdManager -> STDdManager s u
getSTManager (DdManager m) = STDdManager m

cuddReadOne :: DdManager -> DdNode
cuddReadOne (DdManager d) = DdNode $ unsafePerformIO $ do
	node <- c_cuddReadOneWithRef d
	newForeignPtrEnv deref d node

cuddReadLogicZero :: DdManager -> DdNode
cuddReadLogicZero (DdManager d) = DdNode $ unsafePerformIO $ do
	node <- c_cuddReadLogicZeroWithRef d
	newForeignPtrEnv deref d node

cuddBddIthVar :: DdManager -> Int -> DdNode
cuddBddIthVar (DdManager d) i = DdNode $ unsafePerformIO $ do
	node <- c_cuddBddIthVar d (fromIntegral i)
	newForeignPtr_ node

cuddArg1 :: (Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode
cuddArg1 f (DdManager m) (DdNode x) = DdNode $ unsafePerformIO $ 
	withForeignPtr x $ \xp -> do
	node <- f m xp
	newForeignPtrEnv deref m node

cuddArg2 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode -> DdNode
cuddArg2 f (DdManager m) (DdNode l) (DdNode r) = DdNode $ unsafePerformIO $ 
 	withForeignPtr l $ \lp -> 
	withForeignPtr r $ \rp -> do
	node <- f m lp rp
	newForeignPtrEnv deref m node

cuddArg3 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode-> DdNode -> DdNode
cuddArg3 f (DdManager m) (DdNode l) (DdNode r) (DdNode x) = DdNode $ unsafePerformIO $ 
 	withForeignPtr l $ \lp -> 
	withForeignPtr r $ \rp -> 
	withForeignPtr x $ \xp -> do
	node <- f m lp rp xp
	newForeignPtrEnv deref m node

cuddBddAnd :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddAnd = cuddArg2 c_cuddBddAnd

cuddBddOr :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddOr = cuddArg2 c_cuddBddOr

cuddBddNand :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddNand = cuddArg2 c_cuddBddNand

cuddBddNor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddNor = cuddArg2 c_cuddBddNor

cuddBddXor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddXor = cuddArg2 c_cuddBddXor

cuddBddXnor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddXnor = cuddArg2 c_cuddBddXnor

cuddNot :: DdManager -> DdNode -> DdNode
cuddNot = cuddArg1 (const c_cuddNot)

foreign import ccall safe "cuddwrap.h wrappedCuddDumpDot"
	c_cuddDumpDot :: Ptr CDdManager -> Ptr CDdNode -> CString -> IO ()

cuddDumpDot :: DdManager -> DdNode -> String -> IO ()
cuddDumpDot (DdManager m) (DdNode n) s  = 
	withForeignPtr n $ \np -> 
		withCAString s $ \str -> 
			c_cuddDumpDot m np str

foreign import ccall safe "cuddwrap.h wrappedCuddIsComplement"
    c_cuddIsComplement :: Ptr CDdNode -> CInt

cuddEval :: DdManager -> DdNode -> [Int] -> Bool
cuddEval (DdManager m) (DdNode n) a = unsafePerformIO $ do
    res <- withArray (map fromIntegral a) $ \ap -> 
        withForeignPtr n $ \np -> 
            c_cuddEval m np ap
    return $ (==0) $ c_cuddIsComplement res

cuddPrintMinterm :: DdManager -> DdNode -> IO ()
cuddPrintMinterm (DdManager m) (DdNode n) = 
    withForeignPtr n $ c_cuddPrintMinterm m 

foreign import ccall safe "cuddwrap.h allSat"
    c_allSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> Ptr CInt -> IO (Ptr (Ptr CInt))

foreign import ccall safe "cuddwrap.h oneSat"
    c_oneSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

cuddAllSat :: DdManager -> DdNode -> [[SatBit]]
cuddAllSat (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    alloca $ \ntermsptr -> 
    withForeignPtr n $ \np -> do
    res <- c_allSat m np ntermsptr nvarsptr
    nterms <- liftM fromIntegral $ peek ntermsptr
    res <- peekArray nterms res
    nvars <- liftM fromIntegral $ peek nvarsptr
    res <- mapM (peekArray nvars) res
    return $ map (map (toSatBit . fromIntegral)) res

cuddOneSat :: DdManager -> DdNode -> Maybe [SatBit]
cuddOneSat (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr ->
    withForeignPtr n $ \np -> do
    res <- c_oneSat m np nvarsptr
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map (toSatBit . fromIntegral) res

foreign import ccall safe "cuddwrap.h onePrime"
    c_onePrime :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

cuddOnePrime :: DdManager -> DdNode -> DdNode -> Maybe [Int]
cuddOnePrime (DdManager m) (DdNode l) (DdNode u) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    withForeignPtr l $ \lp -> 
    withForeignPtr u $ \up -> do
    res <- c_onePrime m lp up nvarsptr 
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map fromIntegral res

cuddReadSize :: DdManager -> Int
cuddReadSize (DdManager m) = fromIntegral $ unsafePerformIO $ c_cuddReadSize m

cuddSupportIndex :: DdManager -> DdNode -> [Bool]
cuddSupportIndex (DdManager m) (DdNode n) = unsafePerformIO $ 
	withForeignPtr n $ \np -> do
    res <- c_cuddSupportIndex m np
    size <- c_cuddReadSize m
    res <- peekArray (fromIntegral size) res
    return $ map toBool res

cuddBddExistAbstract :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddExistAbstract = cuddArg2 c_cuddBddExistAbstract

cuddBddUnivAbstract :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddUnivAbstract = cuddArg2 c_cuddBddUnivAbstract

cuddBddIte :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
cuddBddIte = cuddArg3 c_cuddBddIte

cuddBddSwapVariables :: DdManager -> DdNode -> [DdNode] -> [DdNode] -> DdNode
cuddBddSwapVariables (DdManager m) (DdNode d) s1 s2 = DdNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withForeignArrayPtrLen (map unDdNode s1) $ \s1 s1ps -> 
    withForeignArrayPtrLen (map unDdNode s2) $ \s2 s2ps -> do
    when (s1 /= s2) (error "cuddBddSwapVariables: variable lists have different sizes")
    node <- c_cuddBddSwapVariables m dp s1ps s2ps (fromIntegral s1)
    newForeignPtrEnv deref m node

cuddBddPermute :: DdManager -> DdNode -> [Int] -> DdNode 
cuddBddPermute (DdManager m) (DdNode d) indexes = DdNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withArray (map fromIntegral indexes) $ \ip -> do
    node <- c_cuddBddPermute m dp ip 
    newForeignPtrEnv deref m node

makePermutArray :: Int -> [Int] -> [Int] -> [Int]
makePermutArray size x y = elems $ accumArray (flip const) 0 (0, size-1) ascList
    where
    ascList = [(i, i) | i <- [0..size-1]] ++ zip x y 

cuddBddShift :: DdManager -> DdNode -> [Int] -> [Int] -> DdNode
cuddBddShift (DdManager m) (DdNode d) from to = DdNode $ unsafePerformIO $
    withForeignPtr d $ \dp -> do
    dp <- evaluate dp
    size <- c_cuddReadSize m 
    let perm = makePermutArray (fromIntegral size) from to
    withArray (map fromIntegral perm) $ \pp -> do
        node <- c_cuddBddPermute m dp pp
        newForeignPtrEnv deref m node

cuddXgty :: DdManager -> [DdNode] -> [DdNode] -> DdNode
cuddXgty (DdManager m) x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXgty: variable lists have different sizes")
    node <- c_cuddXgty m (fromIntegral xl) nullPtr xp yp
    newForeignPtrEnv deref m node

cuddXeqy :: DdManager -> [DdNode] -> [DdNode] -> DdNode
cuddXeqy (DdManager m) x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXeqy: variable lists have different sizes")
    node <- c_cuddXeqy m (fromIntegral xl) xp yp
    newForeignPtrEnv deref m node

cuddInequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
cuddInequality (DdManager m) n c x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtr (map unDdNode x) $ \xp -> 
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddInequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

cuddDisequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
cuddDisequality (DdManager m) n c x y = DdNode $ unsafePerformIO $
    withForeignArrayPtr (map unDdNode x) $ \xp -> 
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddDisequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

cuddBddInterval :: DdManager -> [DdNode] -> Int -> Int -> DdNode
cuddBddInterval (DdManager m) vararr lower upper =  DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode vararr) $ \sz vp -> do
    node <- c_cuddBddInterval m (fromIntegral sz) vp (fromIntegral lower) (fromIntegral upper)
    newForeignPtrEnv deref m node

cuddNodeReadIndex :: DdNode -> Int
cuddNodeReadIndex (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddNodeReadIndex 

cuddDagSize (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddDagSize 

cuddIndicesToCube :: DdManager -> [Int] -> DdNode
cuddIndicesToCube (DdManager m) indices = DdNode $ unsafePerformIO $ 
    withArrayLen (map fromIntegral indices) $ \size ip -> do
    node <- c_cuddIndicesToCube m ip (fromIntegral size)
    newForeignPtrEnv deref m node

cuddBddLICompaction :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddLICompaction = cuddArg2 c_cuddBddLICompaction

cuddBddMinimize :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddMinimize = cuddArg2 c_cuddBddMinimize

--Bdd implication
cuddBddImp :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddImp m l r = cuddBddOr m (cuddNot m l) r

cuddBddPickOneMinterm :: DdManager -> DdNode -> [DdNode] -> Maybe DdNode
cuddBddPickOneMinterm (DdManager m) (DdNode d) vars = unsafePerformIO $
	withForeignPtr d $ \dp -> 
	withForeignArrayPtrLen (map unDdNode vars) $ \vs vp -> do
	node <- c_cuddBddPickOneMinterm m dp vp (fromIntegral vs)
	if node == nullPtr then return Nothing else do
		nd <- newForeignPtrEnv deref m node
		return $ Just $ DdNode nd

foreign import ccall safe "cudd.h Cudd_PrintInfo"
       c_cuddPrintInfo :: Ptr CDdManager -> Ptr CFile -> IO CInt
                
cuddPrintInfo :: DdManager -> Ptr CFile -> IO Int
cuddPrintInfo (DdManager m) cf = liftM fromIntegral $ c_cuddPrintInfo m cf

cuddReadPerm :: DdManager -> Int -> Int
cuddReadPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadPerm m (fromIntegral i)

cuddReadInvPerm :: DdManager -> Int -> Int
cuddReadInvPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadInvPerm m (fromIntegral i)

cuddReadPerms :: DdManager -> [Int]
cuddReadPerms m = map (cuddReadPerm m) [0..(cuddReadSize m - 1)]

cuddReadInvPerms :: DdManager -> [Int]
cuddReadInvPerms m = map (cuddReadInvPerm m) [0..(cuddReadSize m -1)]

cuddReadTree :: DdManager -> IO (Ptr CMtrNode) 
cuddReadTree (DdManager m) = c_cuddReadTree m

cuddCountLeaves :: DdNode -> Int
cuddCountLeaves (DdNode d) = fromIntegral $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    c_cuddCountLeaves dp

cuddCountMinterm :: DdManager -> DdNode -> Int -> Double
cuddCountMinterm (DdManager m) (DdNode d) n = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountMinterm m dp (fromIntegral n) 

cuddCountPathsToNonZero :: DdNode -> Double
cuddCountPathsToNonZero (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountPathsToNonZero dp

cuddCountPath :: DdNode -> Double
cuddCountPath (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountPath dp

foreign import ccall safe "cudd.h Cudd_PrintDebug"
    c_cuddPrintDebug :: Ptr CDdManager -> Ptr CDdNode -> CInt -> CInt -> IO CInt

cuddPrintDebug :: DdManager -> DdNode -> Int -> Int -> IO Int
cuddPrintDebug (DdManager m) (DdNode d) n pr = liftM fromIntegral $ 
    withForeignPtr d $ \dp -> 
    c_cuddPrintDebug m dp (fromIntegral n) (fromIntegral pr)

cuddBddAndAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode  
cuddBddAndAbstract = cuddArg3 c_cuddBddAndAbstract

cuddBddXorExistAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode  
cuddBddXorExistAbstract = cuddArg3 c_cuddBddXorExistAbstract

cuddBddTransfer :: DdManager -> DdManager -> DdNode -> DdNode
cuddBddTransfer (DdManager m1) (DdManager m2) (DdNode x) = DdNode $ unsafePerformIO $ 
    withForeignPtr x $ \xp -> do
        node <- c_cuddBddTransfer m1 m2 xp
        newForeignPtrEnv deref m2 node

cuddBddMakePrime :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddMakePrime = cuddArg2 c_cuddBddMakePrime

cuddBddConstrain :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddConstrain = cuddArg2 c_cuddBddConstrain

cuddBddRestrict :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddRestrict = cuddArg2 c_cuddBddRestrict

cuddBddSqueeze :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddSqueeze = cuddArg2 c_cuddBddSqueeze

cuddLargestCube :: DdManager -> DdNode -> (Int, DdNode)
cuddLargestCube (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \lp ->
    withForeignPtr n $ \np -> do
    node <- c_cuddLargestCube m np lp
    res <- newForeignPtrEnv deref m node
    l <- peek lp
    return (fromIntegral l, DdNode res)
     
cuddBddLeq :: DdManager -> DdNode -> DdNode -> Bool
cuddBddLeq (DdManager m) (DdNode l) (DdNode r) = (==1) $ unsafePerformIO $ 
    withForeignPtr l $ \lp -> 
    withForeignPtr r $ \rp ->
    c_cuddBddLeq m lp rp

cuddCheckKeys :: DdManager -> ST s Int
cuddCheckKeys (DdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddCheckKeys m

cuddDebugCheck :: DdManager -> ST s Int
cuddDebugCheck (DdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddDebugCheck m

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode 

