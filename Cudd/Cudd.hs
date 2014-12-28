{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

module Cudd.Cudd (
    DdManager(),
    DdNode(),
    cuddInit,
    cuddInitOrder,
    readOne,
    readLogicZero,
    ithVar,
    bAnd,
    bOr,
    bNand,
    bNor,
    bXor,
    bXnor,
    bNot,
    dumpDot,
    cudd_cache_slots,
    cudd_unique_slots,
    eval,
    printMinterm,
    allSat,
    oneSat,
    onePrime,
    supportIndex,
    bExists,
    bForall,
    bIte,
    permute,
    shift,
    swapVariables,
    nodeReadIndex,
    dagSize,
    indicesToCube,
    getManagerST,
    getSTManager,
    liCompaction,
    minimize,
    readSize,
    xEqY,
    xGtY,
    interval,
    disequality,
    inequality,
    ddNodeToInt,
    pickOneMinterm,
    readPerm,
    readInvPerm,
    readPerms,
    readInvPerms,
    readTree,
    countLeaves,
    countMinterm,
    countPath,
    countPathsToNonZero,
    printDebug,
    andAbstract,
    xorExistAbstract,
    transfer,
    makePrime,
    constrain,
    restrict,
    squeeze,
    SatBit(..),
    largestCube,
    lEq,
    printInfo
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

cuddArg0 :: (Ptr CDdManager -> IO (Ptr CDdNode)) -> DdManager -> DdNode
cuddArg0 f (DdManager m) = DdNode $ unsafePerformIO $ do
    node <- f m 
    newForeignPtrEnv deref m node

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

readOne :: DdManager -> DdNode
readOne = cuddArg0 c_cuddReadOneWithRef

readLogicZero :: DdManager -> DdNode
readLogicZero  = cuddArg0 c_cuddReadLogicZeroWithRef

ithVar :: DdManager -> Int -> DdNode
ithVar m i = cuddArg0 (flip c_cuddBddIthVar (fromIntegral i)) m

bAnd :: DdManager -> DdNode -> DdNode -> DdNode
bAnd = cuddArg2 c_cuddBddAnd

bOr :: DdManager -> DdNode -> DdNode -> DdNode
bOr = cuddArg2 c_cuddBddOr

bNand :: DdManager -> DdNode -> DdNode -> DdNode
bNand = cuddArg2 c_cuddBddNand

bNor :: DdManager -> DdNode -> DdNode -> DdNode
bNor = cuddArg2 c_cuddBddNor

bXor :: DdManager -> DdNode -> DdNode -> DdNode
bXor = cuddArg2 c_cuddBddXor

bXnor :: DdManager -> DdNode -> DdNode -> DdNode
bXnor = cuddArg2 c_cuddBddXnor

bNot :: DdManager -> DdNode -> DdNode
bNot = cuddArg1 (const c_cuddNot)

foreign import ccall safe "cuddwrap.h wrappedCuddDumpDot"
	c_cuddDumpDot :: Ptr CDdManager -> Ptr CDdNode -> CString -> IO ()

dumpDot :: DdManager -> DdNode -> String -> IO ()
dumpDot (DdManager m) (DdNode n) s  = 
	withForeignPtr n $ \np -> 
		withCAString s $ \str -> 
			c_cuddDumpDot m np str

foreign import ccall safe "cuddwrap.h wrappedCuddIsComplement"
    c_cuddIsComplement :: Ptr CDdNode -> CInt

eval :: DdManager -> DdNode -> [Int] -> Bool
eval (DdManager m) (DdNode n) a = unsafePerformIO $ do
    res <- withArray (map fromIntegral a) $ \ap -> 
        withForeignPtr n $ \np -> 
            c_cuddEval m np ap
    return $ (==0) $ c_cuddIsComplement res

printMinterm :: DdManager -> DdNode -> IO ()
printMinterm (DdManager m) (DdNode n) = 
    withForeignPtr n $ c_cuddPrintMinterm m 

foreign import ccall safe "cuddwrap.h allSat"
    c_allSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> Ptr CInt -> IO (Ptr (Ptr CInt))

foreign import ccall safe "cuddwrap.h oneSat"
    c_oneSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

allSat :: DdManager -> DdNode -> [[SatBit]]
allSat (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    alloca $ \ntermsptr -> 
    withForeignPtr n $ \np -> do
    res <- c_allSat m np ntermsptr nvarsptr
    nterms <- liftM fromIntegral $ peek ntermsptr
    res <- peekArray nterms res
    nvars <- liftM fromIntegral $ peek nvarsptr
    res <- mapM (peekArray nvars) res
    return $ map (map (toSatBit . fromIntegral)) res

oneSat :: DdManager -> DdNode -> Maybe [SatBit]
oneSat (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr ->
    withForeignPtr n $ \np -> do
    res <- c_oneSat m np nvarsptr
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map (toSatBit . fromIntegral) res

foreign import ccall safe "cuddwrap.h onePrime"
    c_onePrime :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

onePrime :: DdManager -> DdNode -> DdNode -> Maybe [Int]
onePrime (DdManager m) (DdNode l) (DdNode u) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    withForeignPtr l $ \lp -> 
    withForeignPtr u $ \up -> do
    res <- c_onePrime m lp up nvarsptr 
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map fromIntegral res

readSize :: DdManager -> Int
readSize (DdManager m) = fromIntegral $ unsafePerformIO $ c_cuddReadSize m

supportIndex :: DdManager -> DdNode -> [Bool]
supportIndex (DdManager m) (DdNode n) = unsafePerformIO $ 
	withForeignPtr n $ \np -> do
    res <- c_cuddSupportIndex m np
    size <- c_cuddReadSize m
    res <- peekArray (fromIntegral size) res
    return $ map toBool res

bExists :: DdManager -> DdNode -> DdNode -> DdNode
bExists = cuddArg2 c_cuddBddExistAbstract

bForall :: DdManager -> DdNode -> DdNode -> DdNode
bForall = cuddArg2 c_cuddBddUnivAbstract

bIte :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
bIte = cuddArg3 c_cuddBddIte

swapVariables :: DdManager -> DdNode -> [DdNode] -> [DdNode] -> DdNode
swapVariables (DdManager m) (DdNode d) s1 s2 = DdNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withForeignArrayPtrLen (map unDdNode s1) $ \s1 s1ps -> 
    withForeignArrayPtrLen (map unDdNode s2) $ \s2 s2ps -> do
    when (s1 /= s2) (error "cuddBddSwapVariables: variable lists have different sizes")
    node <- c_cuddBddSwapVariables m dp s1ps s2ps (fromIntegral s1)
    newForeignPtrEnv deref m node

permute :: DdManager -> DdNode -> [Int] -> DdNode 
permute (DdManager m) (DdNode d) indexes = DdNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withArray (map fromIntegral indexes) $ \ip -> do
    node <- c_cuddBddPermute m dp ip 
    newForeignPtrEnv deref m node

makePermutArray :: Int -> [Int] -> [Int] -> [Int]
makePermutArray size x y = elems $ accumArray (flip const) 0 (0, size-1) ascList
    where
    ascList = [(i, i) | i <- [0..size-1]] ++ zip x y 

shift :: DdManager -> DdNode -> [Int] -> [Int] -> DdNode
shift (DdManager m) (DdNode d) from to = DdNode $ unsafePerformIO $
    withForeignPtr d $ \dp -> do
    dp <- evaluate dp
    size <- c_cuddReadSize m 
    let perm = makePermutArray (fromIntegral size) from to
    withArray (map fromIntegral perm) $ \pp -> do
        node <- c_cuddBddPermute m dp pp
        newForeignPtrEnv deref m node

xGtY :: DdManager -> [DdNode] -> [DdNode] -> DdNode
xGtY (DdManager m) x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXgty: variable lists have different sizes")
    node <- c_cuddXgty m (fromIntegral xl) nullPtr xp yp
    newForeignPtrEnv deref m node

xEqY :: DdManager -> [DdNode] -> [DdNode] -> DdNode
xEqY (DdManager m) x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXeqy: variable lists have different sizes")
    node <- c_cuddXeqy m (fromIntegral xl) xp yp
    newForeignPtrEnv deref m node

inequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
inequality (DdManager m) n c x y = DdNode $ unsafePerformIO $ 
    withForeignArrayPtr (map unDdNode x) $ \xp -> 
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddInequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

disequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
disequality (DdManager m) n c x y = DdNode $ unsafePerformIO $
    withForeignArrayPtr (map unDdNode x) $ \xp -> 
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddDisequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

interval :: DdManager -> [DdNode] -> Int -> Int -> DdNode
interval (DdManager m) vararr lower upper =  DdNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDdNode vararr) $ \sz vp -> do
    node <- c_cuddBddInterval m (fromIntegral sz) vp (fromIntegral lower) (fromIntegral upper)
    newForeignPtrEnv deref m node

nodeReadIndex :: DdNode -> Int
nodeReadIndex (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddNodeReadIndex 

dagSize :: DdNode -> Int
dagSize (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddDagSize 

indicesToCube :: DdManager -> [Int] -> DdNode
indicesToCube (DdManager m) indices = DdNode $ unsafePerformIO $ 
    withArrayLen (map fromIntegral indices) $ \size ip -> do
    node <- c_cuddIndicesToCube m ip (fromIntegral size)
    newForeignPtrEnv deref m node

liCompaction :: DdManager -> DdNode -> DdNode -> DdNode
liCompaction = cuddArg2 c_cuddBddLICompaction

minimize :: DdManager -> DdNode -> DdNode -> DdNode
minimize = cuddArg2 c_cuddBddMinimize

pickOneMinterm :: DdManager -> DdNode -> [DdNode] -> Maybe DdNode
pickOneMinterm (DdManager m) (DdNode d) vars = unsafePerformIO $
	withForeignPtr d $ \dp -> 
	withForeignArrayPtrLen (map unDdNode vars) $ \vs vp -> do
	node <- c_cuddBddPickOneMinterm m dp vp (fromIntegral vs)
	if node == nullPtr then return Nothing else do
		nd <- newForeignPtrEnv deref m node
		return $ Just $ DdNode nd

printInfo :: DdManager -> Ptr CFile -> IO Int
printInfo (DdManager m) cf = liftM fromIntegral $ c_cuddPrintInfo m cf

readPerm :: DdManager -> Int -> Int
readPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadPerm m (fromIntegral i)

readInvPerm :: DdManager -> Int -> Int
readInvPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadInvPerm m (fromIntegral i)

readPerms :: DdManager -> [Int]
readPerms m = map (readPerm m) [0..(readSize m - 1)]

readInvPerms :: DdManager -> [Int]
readInvPerms m = map (readInvPerm m) [0..(readSize m -1)]

readTree :: DdManager -> IO (Ptr CMtrNode) 
readTree (DdManager m) = c_cuddReadTree m

countLeaves :: DdNode -> Int
countLeaves (DdNode d) = fromIntegral $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    c_cuddCountLeaves dp

countMinterm :: DdManager -> DdNode -> Int -> Double
countMinterm (DdManager m) (DdNode d) n = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountMinterm m dp (fromIntegral n) 

countPathsToNonZero :: DdNode -> Double
countPathsToNonZero (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountPathsToNonZero dp

countPath :: DdNode -> Double
countPath (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountPath dp

printDebug :: DdManager -> DdNode -> Int -> Int -> IO Int
printDebug (DdManager m) (DdNode d) n pr = liftM fromIntegral $ 
    withForeignPtr d $ \dp -> 
    c_cuddPrintDebug m dp (fromIntegral n) (fromIntegral pr)

andAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode  
andAbstract = cuddArg3 c_cuddBddAndAbstract

xorExistAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode  
xorExistAbstract = cuddArg3 c_cuddBddXorExistAbstract

transfer :: DdManager -> DdManager -> DdNode -> DdNode
transfer (DdManager m1) (DdManager m2) (DdNode x) = DdNode $ unsafePerformIO $ 
    withForeignPtr x $ \xp -> do
        node <- c_cuddBddTransfer m1 m2 xp
        newForeignPtrEnv deref m2 node

makePrime :: DdManager -> DdNode -> DdNode -> DdNode
makePrime = cuddArg2 c_cuddBddMakePrime

constrain :: DdManager -> DdNode -> DdNode -> DdNode
constrain = cuddArg2 c_cuddBddConstrain

restrict :: DdManager -> DdNode -> DdNode -> DdNode
restrict = cuddArg2 c_cuddBddRestrict

squeeze :: DdManager -> DdNode -> DdNode -> DdNode
squeeze = cuddArg2 c_cuddBddSqueeze

largestCube :: DdManager -> DdNode -> (Int, DdNode)
largestCube (DdManager m) (DdNode n) = unsafePerformIO $ 
    alloca $ \lp ->
    withForeignPtr n $ \np -> do
    node <- c_cuddLargestCube m np lp
    res <- newForeignPtrEnv deref m node
    l <- peek lp
    return (fromIntegral l, DdNode res)
     
lEq :: DdManager -> DdNode -> DdNode -> Bool
lEq (DdManager m) (DdNode l) (DdNode r) = (==1) $ unsafePerformIO $ 
    withForeignPtr l $ \lp -> 
    withForeignPtr r $ \rp ->
    c_cuddBddLeq m lp rp

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode 

