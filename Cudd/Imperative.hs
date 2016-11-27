{-# LANGUAGE RankNTypes #-}

{-| An ST Monad based interface to the CUDD BDD library

This is a straightforward wrapper around the C library. See <http://vlsi.colorado.edu/~fabio/CUDD/> for documentation.

Exampe usage:

> import Control.Monad.ST
> import Cudd.Imperative
> 
> main = do
>     res <- stToIO $ withManagerDefaults $ \manager -> do
>         v1      <- ithVar manager 0
>         v2      <- ithVar manager 1
>         conj    <- bAnd manager v1 v2
>         implies <- lEq manager conj v1
>         deref manager conj
>         return implies
>     print res
-}

module Cudd.Imperative (
    DDManager(..),
    DDNode(..),
    cuddInit,
    cuddInitDefaults,
    withManager, 
    withManagerDefaults,
    withManagerIO,
    withManagerIODefaults,
    shuffleHeap,
    bZero,
    bOne,
    ithVar,
    bAnd,
    bOr,
    bNand,
    bNor,
    bXor,
    bXnor,
    bNot,
    bIte,
    bExists,
    bForall,
    deref,
    setVarMap,
    varMap,
    lEq,
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
    xEqY,
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
    countMintermExact,
    checkCube,
    Cube,
    Prime,
    DDGen(..),
    genFree,
    isGenEmpty,
    firstCube,
    nextCube,
    firstPrime,
    nextPrime,
    firstNode,
    nextNode,
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
import Cudd.MTR
import Cudd.Common

newtype DDManager s u = DDManager {unDDManager :: Ptr CDDManager}
newtype DDNode    s u = DDNode    {unDDNode    :: Ptr CDDNode} deriving (Ord, Eq, Show)

cuddInit :: Int -> Int -> Int -> Int -> Int -> ST s (DDManager s u)
cuddInit numVars numVarsZ numSlots cacheSize maxMemory = unsafeIOToST $ do
    cm <- c_cuddInit (fromIntegral numVars) (fromIntegral numVarsZ) (fromIntegral numSlots) (fromIntegral cacheSize) (fromIntegral maxMemory)
    return $ DDManager cm

cuddInitDefaults :: ST s (DDManager s u)
cuddInitDefaults = cuddInit 0 0 cudd_unique_slots cudd_cache_slots 0

withManager :: Int -> Int -> Int -> Int -> Int -> (forall u. DDManager s u -> ST s a) -> ST s a
withManager numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- cuddInit numVars numVarsZ numSlots cacheSize maxMemory
    f res 

withManagerDefaults :: (forall u. DDManager s u -> ST s a) -> ST s a
withManagerDefaults f = do
    res <- cuddInitDefaults
    f res 

withManagerIO :: MonadIO m => Int -> Int -> Int -> Int -> Int -> (forall u. DDManager RealWorld u -> m a) -> m a
withManagerIO numVars numVarsZ numSlots cacheSize maxMemory f = do
    res <- liftIO $ stToIO $ cuddInit numVars numVarsZ numSlots cacheSize maxMemory
    f res

withManagerIODefaults :: MonadIO m => (forall u. DDManager RealWorld u -> m a) -> m a
withManagerIODefaults f = do
    res <- liftIO $ stToIO cuddInitDefaults
    f res

shuffleHeap :: DDManager s u -> [Int] -> ST s ()
shuffleHeap (DDManager m) order = unsafeIOToST $ 
    withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "shuffleHeap: order does not contain each variable once") 
    res1 <- c_cuddBddIthVar m (fromIntegral (size - 1))
    when (res1 == nullPtr) (error "shuffleHeap: Failed to resize table")
    res2 <- c_cuddShuffleHeap m ptr
    when (fromIntegral res2 /= 1) (error "shuffleHeap: Cudd_ShuffleHeap failed")
    return ()

toInt :: DDNode s u -> Int
toInt (DDNode n) = fromIntegral $ ptrToIntPtr n

arg0 :: (Ptr CDDManager -> IO (Ptr CDDNode)) -> DDManager s u -> ST s (DDNode s u)
arg0 f (DDManager m) = liftM DDNode $ unsafeIOToST $ f m

arg1 :: (Ptr CDDManager -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager s u -> DDNode s u -> ST s (DDNode s u)
arg1 f (DDManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ f m x

arg2 :: (Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager s u -> DDNode s u -> DDNode s u -> ST s (DDNode s u)
arg2 f (DDManager m) (DDNode x) (DDNode y) = liftM DDNode $ unsafeIOToST $ f m x y
    
arg3 :: (Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager s u -> DDNode s u -> DDNode s u -> DDNode s u  -> ST s (DDNode s u)
arg3 f (DDManager m) (DDNode x) (DDNode y) (DDNode z) = liftM DDNode $ unsafeIOToST $ f m x y z

bZero, bOne :: DDManager s u -> DDNode s u
bZero (DDManager m) = DDNode $ unsafePerformIO $ c_cuddReadLogicZero m
bOne  (DDManager m) = DDNode $ unsafePerformIO $ c_cuddReadOne m

bAnd    = arg2 c_cuddBddAnd
bOr     = arg2 c_cuddBddOr
bNand   = arg2 c_cuddBddNand
bNor    = arg2 c_cuddBddNor
bXor    = arg2 c_cuddBddXor
bXnor   = arg2 c_cuddBddXnor
bIte    = arg3 c_cuddBddIte
bExists = arg2 c_cuddBddExistAbstract
bForall = arg2 c_cuddBddUnivAbstract

andAbstract      = arg3 c_cuddBddAndAbstract
xorExistAbstract = arg3 c_cuddBddXorExistAbstract

bNot :: DDNode s u -> DDNode s u
bNot (DDNode x) = DDNode $ unsafePerformIO $ c_cuddNotNoRef x

ithVar :: DDManager s u -> Int -> ST s (DDNode s u)
ithVar (DDManager m) i = liftM DDNode $ unsafeIOToST $ c_cuddBddIthVar m (fromIntegral i)

deref :: DDManager s u -> DDNode s u -> ST s ()
deref (DDManager m) (DDNode x) = unsafeIOToST $ c_cuddIterDerefBdd m x

setVarMap :: DDManager s u -> [DDNode s u] -> [DDNode s u] -> ST s ()
setVarMap (DDManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "setVarMap: lengths not equal")
    void $ c_cuddSetVarMap m xp yp (fromIntegral xl)

varMap :: DDManager s u -> DDNode s u -> ST s (DDNode s u)
varMap (DDManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ c_cuddBddVarMap m x

lEq :: DDManager s u -> DDNode s u -> DDNode s u -> ST s Bool
lEq (DDManager m) (DDNode x) (DDNode y) = liftM (==1) $ unsafeIOToST $ c_cuddBddLeq m x y

swapVariables :: DDManager s u -> [DDNode s u] -> [DDNode s u] -> DDNode s u -> ST s (DDNode s u)
swapVariables (DDManager m) nodesx nodesy (DDNode x) = unsafeIOToST $ 
    withArrayLen (map unDDNode nodesx) $ \lx xp -> 
    withArrayLen (map unDDNode nodesy) $ \ly yp -> do
    when (lx /= ly) $ error "CuddExplicitDeref: shift: lengths not equal"
    res <- c_cuddBddSwapVariables m x xp yp (fromIntegral lx)
    return $ DDNode res

ref :: DDNode s u -> ST s ()
ref (DDNode x) = unsafeIOToST $ cuddRef x

largestCube :: DDManager s u -> DDNode s u -> ST s (DDNode s u, Int)
largestCube (DDManager m) (DDNode x) = unsafeIOToST $ 
    alloca $ \lp -> do
    res <- c_cuddLargestCube m x lp 
    l <- peek lp
    return (DDNode res, fromIntegral l)

makePrime :: DDManager s u -> DDNode s u -> DDNode s u -> ST s (DDNode s u)
makePrime = arg2 c_cuddBddMakePrime

support :: DDManager s u -> DDNode s u -> ST s (DDNode s u)
support = arg1 c_cuddSupport

supportIndices :: DDManager s u -> DDNode s u -> ST s [Int]
supportIndices (DDManager m) (DDNode x) = unsafeIOToST $
    alloca $ \arrp -> do
    sz <- c_cuddSupportIndices m x arrp
    aaddr <- peek arrp
    res <- peekArray (fromIntegral sz) aaddr
    return $ map fromIntegral res

indicesToCube :: DDManager s u -> [Int] -> ST s (DDNode s u)
indicesToCube (DDManager m) indices = unsafeIOToST $
    withArrayLen (map fromIntegral indices) $ \sz pt -> do
    res <- c_cuddIndicesToCube m pt (fromIntegral sz)
    return $ DDNode res

computeCube :: DDManager s u -> [DDNode s u] -> [Bool] -> ST s (DDNode s u)
computeCube (DDManager m) nodes phases = unsafeIOToST $ 
    withArrayLen (map unDDNode nodes) $ \szn ptn -> 
    withArrayLen (map (fromIntegral . fromBool) phases) $ \szp ptp -> do
    when (szn /= szp) $ error "computeCube: lists are different lengths"
    res <- c_cuddBddComputeCube m ptn ptp (fromIntegral szn)
    return $ DDNode res

nodesToCube :: DDManager s u -> [DDNode s u] -> ST s (DDNode s u)
nodesToCube (DDManager m) nodes = unsafeIOToST $
    withArrayLen (map unDDNode nodes) $ \sz pt -> do
    res <- c_cuddBddComputeCube m pt nullPtr (fromIntegral sz)
    return $ DDNode res

readSize :: DDManager s u -> ST s Int
readSize (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadSize m

bddToCubeArray :: DDManager s u -> DDNode s u -> ST s [SatBit]
bddToCubeArray ma@(DDManager m) (DDNode x) = unsafeIOToST $ do
    size <- liftM fromIntegral $ c_cuddReadSize m
    allocaArray size $ \resptr -> do
        c_cuddBddToCubeArray m x resptr
        res <- peekArray size resptr
        return $ map (toSatBit . fromIntegral) res

compose :: DDManager s u -> DDNode s u -> DDNode s u -> Int -> ST s (DDNode s u)
compose (DDManager m) (DDNode f) (DDNode g) v = liftM DDNode $ unsafeIOToST $ c_cuddBddCompose m f g (fromIntegral v)

arg3Bool :: (Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO CInt) -> DDManager s u -> DDNode s u -> DDNode s u -> DDNode s u  -> ST s Bool
arg3Bool f (DDManager m) (DDNode x) (DDNode y) (DDNode z) = liftM (==1) $ unsafeIOToST $ f m x y z

leqUnless, equivDC :: DDManager s u -> DDNode s u -> DDNode s u -> DDNode s u -> ST s Bool
leqUnless = arg3Bool c_cuddBddLeqUnless
equivDC   = arg3Bool c_cuddEquivDC

xEqY :: DDManager s u -> [DDNode s u] -> [DDNode s u] -> ST s (DDNode s u)
xEqY (DDManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "xeqy: lengths not equal")
    res <- c_cuddXeqy m (fromIntegral xl) xp yp
    return $ DDNode res

debugCheck :: DDManager s u -> ST s Int
debugCheck (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddDebugCheck m

checkKeys :: DDManager s u -> ST s Int
checkKeys (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddCheckKeys m

pickOneMinterm :: DDManager s u -> DDNode s u -> [DDNode s u] -> ST s (DDNode s u)
pickOneMinterm (DDManager m) (DDNode d) vars = unsafeIOToST $ 
    withArrayLen (map unDDNode vars) $ \vl vp -> do
        res <- c_cuddBddPickOneMinterm m d vp (fromIntegral vl)
        return $ DDNode res

checkZeroRef :: DDManager s u -> ST s Int
checkZeroRef (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddCheckZeroRef m

readInvPerm :: DDManager s u -> Int -> ST s Int
readInvPerm (DDManager m) offs = liftM fromIntegral $ unsafeIOToST $ c_cuddReadInvPerm m (fromIntegral offs)

readPerm :: DDManager s u -> Int -> ST s Int
readPerm (DDManager m) offs = liftM fromIntegral $ unsafeIOToST $ c_cuddReadPerm m (fromIntegral offs)

dagSize :: DDNode s u -> ST s Int
dagSize (DDNode d) = liftM fromIntegral $ unsafeIOToST $ c_cuddDagSize d

readNodeCount :: DDManager s u -> ST s Integer
readNodeCount (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadNodeCount m

readPeakNodeCount :: DDManager s u -> ST s Integer
readPeakNodeCount (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadPeakNodeCount m

regular :: DDNode s u -> DDNode s u
regular (DDNode x) = DDNode $ unsafePerformIO $ c_wrappedRegular x

readMaxCache :: DDManager s u -> ST s Int
readMaxCache (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadMaxCache m

readMaxCacheHard :: DDManager s u -> ST s Int
readMaxCacheHard (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadMaxCacheHard m

setMaxCacheHard :: DDManager s u -> Int -> ST s ()
setMaxCacheHard (DDManager m) x = unsafeIOToST $ c_cuddSetMaxCacheHard m (fromIntegral x)

readCacheSlots :: DDManager s u -> ST s Int
readCacheSlots (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadCacheSlots m

readCacheUsedSlots :: DDManager s u -> ST s Int
readCacheUsedSlots (DDManager m) = liftM fromIntegral $ unsafeIOToST $ c_cuddReadCacheUsedSlots m

andLimit :: DDManager s u -> DDNode s u -> DDNode s u -> Int -> ST s (Maybe (DDNode s u))
andLimit (DDManager m) (DDNode x) (DDNode y) lim = unsafeIOToST $ do
    res <- c_cuddBddAndLimit m x y (fromIntegral lim)
    if res==nullPtr then
        return Nothing
    else do
        cuddRef res
        return $ Just $ DDNode res

readTree :: DDManager s u -> ST s (MtrNode s)
readTree (DDManager m) = liftM MtrNode $ unsafeIOToST $ c_cuddReadTree m

newVarAtLevel :: DDManager s u -> Int -> ST s (DDNode s u)
newVarAtLevel (DDManager m) level = liftM DDNode $ unsafeIOToST $ c_cuddBddNewVarAtLevel m (fromIntegral level)

liCompaction = arg2 c_cuddBddLICompaction
squeeze      = arg2 c_cuddBddSqueeze
minimize     = arg2 c_cuddBddMinimize

newVar :: DDManager s u -> ST s (DDNode s u)
newVar (DDManager m) = liftM DDNode $ unsafeIOToST $ c_cuddBddNewVar m

vectorCompose :: DDManager s u -> DDNode s u -> [DDNode s u] -> ST s (DDNode s u)
vectorCompose (DDManager m) (DDNode f) nodes = liftM DDNode $ unsafeIOToST $ withArrayLen (map unDDNode nodes) $ \len ptr -> do
    sz <- c_cuddReadSize m
    when (fromIntegral sz /= len) (error "vectorCompose: not one entry for each variable in manager")
    c_cuddBddVectorCompose m f ptr

quit :: DDManager s u -> ST s ()
quit (DDManager m) = unsafeIOToST $ c_cuddQuit m

readIndex :: DDNode s u -> ST s Int
readIndex (DDNode x) = liftM fromIntegral $ unsafeIOToST $ c_cuddNodeReadIndex x

printMinterm :: DDManager s u -> DDNode s u -> ST s ()
printMinterm (DDManager m) (DDNode x) = unsafeIOToST $ c_cuddPrintMinterm m x

countMintermExact :: DDManager s u -> DDNode s u -> Int -> ST s Integer
countMintermExact (DDManager m) (DDNode x) n = unsafeIOToST $
    alloca $ \ sizep -> do
    apa <- c_cuddApaCountMinterm m x (fromIntegral n) sizep
    size <- fromIntegral <$> peek sizep
    digits <- peekArray size apa
    c_cuddFreeApaNumber apa
    return $ foldl ( \ a d -> a * 2^32 + fromIntegral d ) 0 digits

checkCube :: DDManager s u -> DDNode s u -> ST s Bool
checkCube (DDManager m) (DDNode x) = liftM (==1) $ unsafeIOToST $ c_cuddCheckCube m x

data Cube
data Prime
data Node
data DDGen s u t = DDGen (Ptr CDDGen)

genFree :: DDGen s u t -> ST s ()
genFree (DDGen g) = void $ unsafeIOToST $ c_cuddGenFree g

isGenEmpty :: DDGen s u t -> ST s Bool
isGenEmpty (DDGen g) = liftM (==1) $ unsafeIOToST $ c_cuddIsGenEmpty g

firstCube :: DDManager s u -> DDNode s u -> ST s (Maybe ([SatBit], DDGen s u Cube))
firstCube (DDManager m) (DDNode n) = unsafeIOToST $ do
    sz <- c_cuddReadSize m
    alloca $ \cubePP -> 
        alloca $ \valP -> do
            gen <- c_cuddFirstCube m n cubePP valP 
            empty <- c_cuddIsGenEmpty gen
            if empty == 1 then do
                c_cuddGenFree gen
                return Nothing
            else do
                cubeP <- peek cubePP
                cube <- peekArray (fromIntegral sz) cubeP
                return $ Just (map (toSatBit . fromIntegral) cube, DDGen gen)

nextCube :: DDManager s u -> DDGen s u Cube -> ST s (Maybe [SatBit])
nextCube (DDManager m) (DDGen g) = unsafeIOToST $ do
    sz <- c_cuddReadSize m
    alloca $ \cubePP -> 
        alloca $ \valP -> do
            c_cuddNextCube g cubePP valP
            empty <- c_cuddIsGenEmpty g
            if empty == 1 then do
                c_cuddGenFree g
                return Nothing
            else do
                cubeP <- peek cubePP
                cube <- peekArray (fromIntegral sz) cubeP
                return $ Just $ map (toSatBit . fromIntegral) cube

firstPrime :: DDManager s u -> DDNode s u -> DDNode s u -> ST s (Maybe ([SatBit], DDGen s u Prime))
firstPrime (DDManager m) (DDNode x) (DDNode y) = unsafeIOToST $ do
    sz <- c_cuddReadSize m
    alloca $ \cubePP -> do
        gen <- c_cuddFirstPrime m x y cubePP
        empty <- c_cuddIsGenEmpty gen
        if empty == 1 then do
            c_cuddGenFree gen
            return Nothing
        else do
            cubeP <- peek cubePP
            cube <- peekArray (fromIntegral sz) cubeP
            return $ Just (map (toSatBit . fromIntegral) cube, DDGen gen)

nextPrime :: DDManager s u -> DDGen s u Prime -> ST s (Maybe [SatBit])
nextPrime (DDManager m) (DDGen g) = unsafeIOToST $ do
    sz <- c_cuddReadSize m
    alloca $ \cubePP -> do
        c_cuddNextPrime g cubePP 
        empty <- c_cuddIsGenEmpty g
        if empty == 1 then do
            c_cuddGenFree g
            return Nothing
        else do
            cubeP <- peek cubePP
            cube <- peekArray (fromIntegral sz) cubeP
            return $ Just $ map (toSatBit . fromIntegral) cube

firstNode :: DDManager s u -> DDNode s u -> ST s (Maybe (DDNode s u, DDGen s u Node))
firstNode (DDManager m) (DDNode x) = unsafeIOToST $ do
    alloca $ \nodePP -> do
        gen <- c_cuddFirstNode m x nodePP
        empty <- c_cuddIsGenEmpty gen
        if empty == 1 then do
            c_cuddGenFree gen
            return Nothing
        else do
            nodeP <- peek nodePP
            return $ Just (DDNode nodeP, DDGen gen)

nextNode :: DDManager s u -> DDGen s u Node -> ST s (Maybe (DDNode s u))
nextNode (DDManager m) (DDGen g) = unsafeIOToST $ do
    alloca $ \nodePP -> do
        c_cuddNextNode g nodePP
        empty <- c_cuddIsGenEmpty g
        if empty == 1 then do
            c_cuddGenFree g
            return Nothing
        else do
            nodeP <- peek nodePP
            return $ Just $ DDNode nodeP

