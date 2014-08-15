{-# LANGUAGE RankNTypes #-}

module Cudd.Imperative (
    cuddInit,
    cuddInitDefaults,
    withManager, 
    withManagerDefaults,
    withManagerIO,
    withManagerIODefaults,
    shuffleHeap,
    bzero,
    bone,
    ithVar,
    band,
    bor,
    bnand,
    bnor,
    bxor,
    bxnor,
    bnot,
    bite,
    bexists,
    bforall,
    deref,
    setVarMap,
    varMap,
    DDNode,
    STDdManager,
    leq,
    swapVariables,
    ref,
    largestCube,
    makePrime,
    support,
    supportIndices,
    indicesToCube,
    computeCube,
    nodesToCube,
    readSize,
    bddToCubeArray,
    compose,
    andAbstract,
    xorExistAbstract,
    leqUnless,
    equivDC,
    xeqy,
    debugCheck,
    checkKeys,
    pickOneMinterm,
    toInt,
    checkZeroRef,
    readInvPerm,
    readPerm,
    dagSize,
    readNodeCount,
    readPeakNodeCount,
    regular,
    readMaxCache,
    readMaxCacheHard,
    setMaxCacheHard,
    readCacheSlots,
    readCacheUsedSlots,
    cudd_unique_slots,
    cudd_cache_slots,
    andLimit,
    readTree,
    newVarAtLevel,
    liCompaction,
    squeeze,
    minimize,
    newVar,
    vectorCompose,
    quit,
    readIndex,
    printMinterm,
    checkCube,
    module Cudd.Common
    ) where

import Foreign hiding (void)
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.IO.Unsafe

import Cudd.C
import Cudd.Internal hiding (deref)
import Cudd.MTR
import Cudd.Common

cuddInit :: Int -> Int -> Int -> Int -> Int -> ST s (STDdManager s u)
cuddInit numVars numVarsZ numSlots cacheSize maxMemory = unsafeIOToST $ do
    cm <- c_cuddInit (fromIntegral numVars) (fromIntegral numVarsZ) (fromIntegral numSlots) (fromIntegral cacheSize) (fromIntegral maxMemory)
    return $ STDdManager cm

cuddInitDefaults :: ST s (STDdManager s u)
cuddInitDefaults = cuddInit 0 0 cudd_unique_slots cudd_cache_slots 0

withManager :: Int -> Int -> Int -> Int -> Int -> (forall u. STDdManager s u -> ST s a) -> ST s a
withManager numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- cuddInit numVars numVarsZ numSlots cacheSize maxMemory
    f res 

withManagerDefaults :: (forall u. STDdManager s u -> ST s a) -> ST s a
withManagerDefaults f = do
    res <- cuddInitDefaults
    f res 

withManagerIO :: MonadIO m => Int -> Int -> Int -> Int -> Int -> (forall u. STDdManager RealWorld u -> m a) -> m a
withManagerIO numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- liftIO $ stToIO $ cuddInit numVars numVarsZ numSlots cacheSize maxMemory
    f res

withManagerIODefaults :: MonadIO m => (forall u. STDdManager RealWorld u -> m a) -> m a
withManagerIODefaults f = do
    res <- liftIO $ stToIO cuddInitDefaults
    f res

shuffleHeap :: STDdManager s u -> [Int] -> ST s ()
shuffleHeap (STDdManager m) order = unsafeIOToST $ 
    withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "shuffleHeap: order does not contain each variable once") 
    res1 <- c_cuddBddIthVar m (fromIntegral (size - 1))
    when (res1 == nullPtr) (error "shuffleHeap: Failed to resize table")
    res2 <- c_cuddShuffleHeap m ptr
    when (fromIntegral res2 /= 1) (error "shuffleHeap: Cudd_ShuffleHeap failed")
    return ()

toInt :: DDNode s u -> Int
toInt (DDNode n) = fromIntegral $ ptrToIntPtr n

arg0 :: (Ptr CDdManager -> IO (Ptr CDdNode)) -> STDdManager s u -> ST s (DDNode s u)
arg0 f (STDdManager m) = liftM DDNode $ unsafeIOToST $ f m

arg1 :: (Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> ST s (DDNode s u)
arg1 f (STDdManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ f m x

arg2 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> DDNode s u -> ST s (DDNode s u)
arg2 f (STDdManager m) (DDNode x) (DDNode y) = liftM DDNode $ unsafeIOToST $ f m x y
    
arg3 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> STDdManager s u -> DDNode s u -> DDNode s u -> DDNode s u  -> ST s (DDNode s u)
arg3 f (STDdManager m) (DDNode x) (DDNode y) (DDNode z) = liftM DDNode $ unsafeIOToST $ f m x y z

bzero (STDdManager m) = DDNode $ unsafePerformIO $ c_cuddReadLogicZero m
bone  (STDdManager m) = DDNode $ unsafePerformIO $ c_cuddReadOne m

band    = arg2 c_cuddBddAnd
bor     = arg2 c_cuddBddOr
bnand   = arg2 c_cuddBddNand
bnor    = arg2 c_cuddBddNor
bxor    = arg2 c_cuddBddXor
bxnor   = arg2 c_cuddBddXnor
bite    = arg3 c_cuddBddIte
bexists = arg2 c_cuddBddExistAbstract
bforall = arg2 c_cuddBddUnivAbstract

andAbstract      = arg3 c_cuddBddAndAbstract
xorExistAbstract = arg3 c_cuddBddXorExistAbstract

bnot (DDNode x) = DDNode $ unsafePerformIO $ c_cuddNotNoRef x
ithVar (STDdManager m) i = liftM DDNode $ unsafeIOToST $ c_cuddBddIthVar m (fromIntegral i)

deref :: STDdManager s u -> DDNode s u -> ST s ()
deref (STDdManager m) (DDNode x) = unsafeIOToST $ c_cuddIterDerefBdd m x

setVarMap :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> ST s ()
setVarMap (STDdManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "setVarMap: lengths not equal")
    void $ c_cuddSetVarMap m xp yp (fromIntegral xl)

varMap :: STDdManager s u -> DDNode s u -> ST s (DDNode s u)
varMap (STDdManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ c_cuddBddVarMap m x

leq :: STDdManager s u -> DDNode s u -> DDNode s u -> ST s Bool
leq (STDdManager m) (DDNode x) (DDNode y) = liftM (==1) $ unsafeIOToST $ c_cuddBddLeq m x y

swapVariables :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> DDNode s u -> ST s (DDNode s u)
swapVariables (STDdManager m) nodesx nodesy (DDNode x) = unsafeIOToST $ 
    withArrayLen (map unDDNode nodesx) $ \lx xp -> 
    withArrayLen (map unDDNode nodesy) $ \ly yp -> do
    when (lx /= ly) $ error "CuddExplicitDeref: shift: lengths not equal"
    res <- c_cuddBddSwapVariables m x xp yp (fromIntegral lx)
    return $ DDNode res

ref :: DDNode s u -> ST s ()
ref (DDNode x) = unsafeIOToST $ cuddRef x

largestCube :: STDdManager s u -> DDNode s u -> ST s (DDNode s u, Int)
largestCube (STDdManager m) (DDNode x) = unsafeIOToST $ 
    alloca $ \lp -> do
    res <- c_cuddLargestCube m x lp 
    l <- peek lp
    return (DDNode res, fromIntegral l)

makePrime :: STDdManager s u -> DDNode s u -> DDNode s u -> ST s (DDNode s u)
makePrime = arg2 c_cuddBddMakePrime

support :: STDdManager s u -> DDNode s u -> ST s (DDNode s u)
support = arg1 c_cuddSupport

supportIndices :: STDdManager s u -> DDNode s u -> ST s [Int]
supportIndices (STDdManager m) (DDNode x) = unsafeIOToST $
    alloca $ \arrp -> do
    sz <- c_cuddSupportIndices m x arrp
    aaddr <- peek arrp
    res <- peekArray (fromIntegral sz) aaddr
    return $ map fromIntegral res

indicesToCube :: STDdManager s u -> [Int] -> ST s (DDNode s u)
indicesToCube (STDdManager m) indices = unsafeIOToST $
    withArrayLen (map fromIntegral indices) $ \sz pt -> do
    res <- c_cuddIndicesToCube m pt (fromIntegral sz)
    return $ DDNode res

computeCube :: STDdManager s u -> [DDNode s u] -> [Bool] -> ST s (DDNode s u)
computeCube (STDdManager m) nodes phases = unsafeIOToST $ 
    withArrayLen (map unDDNode nodes) $ \szn ptn -> 
    withArrayLen (map (fromIntegral . fromBool) phases) $ \szp ptp -> do
    when (szn /= szp) $ error "computeCube: lists are different lengths"
    res <- c_cuddBddComputeCube m ptn ptp (fromIntegral szn)
    return $ DDNode res

nodesToCube :: STDdManager s u -> [DDNode s u] -> ST s (DDNode s u)
nodesToCube (STDdManager m) nodes = unsafeIOToST $
    withArrayLen (map unDDNode nodes) $ \sz pt -> do
    res <- c_cuddBddComputeCube m pt nullPtr (fromIntegral sz)
    return $ DDNode res

readSize :: STDdManager s u -> ST s Int
readSize (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadSize m

bddToCubeArray :: STDdManager s u -> DDNode s u -> ST s [SatBit]
bddToCubeArray ma@(STDdManager m) (DDNode x) = unsafeIOToST $ do
    size <- liftM fromIntegral $ c_cuddReadSize m
    allocaArray size $ \resptr -> do
        c_cuddBddToCubeArray m x resptr
        res <- peekArray size resptr
        return $ map (toSatBit . fromIntegral) res

compose :: STDdManager s u -> DDNode s u -> DDNode s u -> Int -> ST s (DDNode s u)
compose (STDdManager m) (DDNode f) (DDNode g) v = liftM DDNode $ unsafeIOToST $ c_cuddBddCompose m f g (fromIntegral v)

arg3Bool :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO CInt) -> STDdManager s u -> DDNode s u -> DDNode s u -> DDNode s u  -> ST s Bool
arg3Bool f (STDdManager m) (DDNode x) (DDNode y) (DDNode z) = liftM (==1) $ unsafeIOToST $ f m x y z

leqUnless, equivDC :: STDdManager s u -> DDNode s u -> DDNode s u -> DDNode s u -> ST s Bool
leqUnless = arg3Bool c_cuddBddLeqUnless
equivDC   = arg3Bool c_cuddEquivDC

xeqy :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> ST s (DDNode s u)
xeqy (STDdManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "xeqy: lengths not equal")
    res <- c_cuddXeqy m (fromIntegral xl) xp yp
    return $ DDNode res

debugCheck :: STDdManager s u -> ST s Int
debugCheck (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddDebugCheck m

checkKeys :: STDdManager s u -> ST s Int
checkKeys (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddCheckKeys m

pickOneMinterm :: STDdManager s u -> DDNode s u -> [DDNode s u] -> ST s (DDNode s u)
pickOneMinterm (STDdManager m) (DDNode d) vars = unsafeIOToST $ 
    withArrayLen (map unDDNode vars) $ \vl vp -> do
        res <- c_cuddBddPickOneMinterm m d vp (fromIntegral vl)
        return $ DDNode res

checkZeroRef :: STDdManager s u -> ST s Int
checkZeroRef (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddCheckZeroRef m

readInvPerm :: STDdManager s u -> Int -> ST s Int
readInvPerm (STDdManager m) offs = liftM fromIntegral $ unsafeIOToST $ c_cuddReadInvPerm m (fromIntegral offs)

readPerm :: STDdManager s u -> Int -> ST s Int
readPerm (STDdManager m) offs = liftM fromIntegral $ unsafeIOToST $ c_cuddReadPerm m (fromIntegral offs)

dagSize :: DDNode s u -> ST s Int
dagSize (DDNode d) = liftM fromIntegral $ unsafeIOToST $ c_cuddDagSize d

readNodeCount :: STDdManager s u -> ST s Integer
readNodeCount (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadNodeCount m

readPeakNodeCount :: STDdManager s u -> ST s Integer
readPeakNodeCount (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadPeakNodeCount m

regular :: DDNode s u -> DDNode s u
regular (DDNode x) = DDNode $ unsafePerformIO $ c_wrappedRegular x

readMaxCache :: STDdManager s u -> ST s Int
readMaxCache (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadMaxCache m

readMaxCacheHard :: STDdManager s u -> ST s Int
readMaxCacheHard (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadMaxCacheHard m

setMaxCacheHard :: STDdManager s u -> Int -> ST s ()
setMaxCacheHard (STDdManager m) x = unsafeIOToST $ c_cuddSetMaxCacheHard m (fromIntegral x)

readCacheSlots :: STDdManager s u -> ST s Int
readCacheSlots (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadCacheSlots m

readCacheUsedSlots :: STDdManager s u -> ST s Int
readCacheUsedSlots (STDdManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadCacheUsedSlots m

andLimit :: STDdManager s u -> DDNode s u -> DDNode s u -> Int -> ST s (Maybe (DDNode s u))
andLimit (STDdManager m) (DDNode x) (DDNode y) lim = unsafeIOToST $ do
    res <- c_cuddBddAndLimit m x y (fromIntegral lim)
    if res==nullPtr then
        return Nothing
    else do
        cuddRef res
        return $ Just $ DDNode res

readTree :: STDdManager s u -> ST s (MtrNode s)
readTree (STDdManager m) = liftM MtrNode $ unsafeIOToST $ c_cuddReadTree m

newVarAtLevel :: STDdManager s u -> Int -> ST s (DDNode s u)
newVarAtLevel (STDdManager m) level = liftM DDNode $ unsafeIOToST $ c_cuddBddNewVarAtLevel m (fromIntegral level)

liCompaction = arg2 c_cuddBddLICompaction
squeeze      = arg2 c_cuddBddSqueeze
minimize     = arg2 c_cuddBddMinimize

newVar :: STDdManager s u -> ST s (DDNode s u)
newVar (STDdManager m) = liftM DDNode $ unsafeIOToST $ c_cuddBddNewVar m

vectorCompose :: STDdManager s u -> DDNode s u -> [DDNode s u] -> ST s (DDNode s u)
vectorCompose (STDdManager m) (DDNode f) nodes = liftM DDNode $ unsafeIOToST $ withArrayLen (map unDDNode nodes) $ \len ptr -> do
    sz <- c_cuddReadSize m
    when (fromIntegral sz /= len) (error "vectorCompose: not one entry for each variable in manager")
    c_cuddBddVectorCompose m f ptr

quit :: STDdManager s u -> ST s ()
quit (STDdManager m) = unsafeIOToST $ c_cuddQuit m

readIndex :: DDNode s u -> ST s Int
readIndex (DDNode x) = liftM fromIntegral $ unsafeIOToST $ c_cuddNodeReadIndex x

printMinterm :: STDdManager s u -> DDNode s u -> ST s ()
printMinterm (STDdManager m) (DDNode x) = unsafeIOToST $ c_cuddPrintMinterm m x

checkCube :: STDdManager s u -> DDNode s u -> ST s Bool
checkCube (STDdManager m) (DDNode x) = liftM (==1) $ unsafeIOToST $ c_cuddCheckCube m x

