{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

{-| Bindings to the CUDD BDD library

This is a straightforward wrapper around the C library. See <http://vlsi.colorado.edu/~fabio/CUDD/> for documentation.

Exampe usage:

> import Cudd.Cudd
> 
> main = do
>     let manager = cuddInit
>         v1      = ithVar manager 0
>         v2      = ithVar manager 1
>         conj    = bAnd manager v1 v2
>         implies = lEq manager conj v1
>     print implies
-}

module Cudd.Cudd (
    DDManager(..),
    DDNode(..),
    deref,
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
    dumpDot',
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
    swapVariables,
    nodeReadIndex,
    dagSize,
    indicesToCube,
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
import Control.Monad
import Data.List

import Cudd.ForeignHelpers
import Cudd.MTR
import Cudd.C
import Cudd.Common

newtype DDManager = DDManager (Ptr CDDManager)
newtype DDNode    = DDNode    {unDDNode :: ForeignPtr CDDNode} deriving (Ord, Eq, Show)

deref = c_cuddIterDerefBddPtr

cuddInit :: DDManager
cuddInit = DDManager $ unsafePerformIO $ c_cuddInit 0 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0

cuddInitOrder :: [Int] -> DDManager
cuddInitOrder order = DDManager $ unsafePerformIO $ withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "cuddInitOrder: order does not contain each variable once") 
    m <- c_cuddInit (fromIntegral size) 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0
    res <- c_cuddShuffleHeap m ptr
    when (res /= 1) (error "shuffleHeap failed")
    return m

cuddArg0 :: (Ptr CDDManager -> IO (Ptr CDDNode)) -> DDManager -> DDNode
cuddArg0 f (DDManager m) = DDNode $ unsafePerformIO $ do
    node <- f m 
    newForeignPtrEnv deref m node

cuddArg1 :: (Ptr CDDManager -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager -> DDNode -> DDNode
cuddArg1 f (DDManager m) (DDNode x) = DDNode $ unsafePerformIO $ 
	withForeignPtr x $ \xp -> do
	node <- f m xp
	newForeignPtrEnv deref m node

cuddArg2 :: (Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager -> DDNode -> DDNode -> DDNode
cuddArg2 f (DDManager m) (DDNode l) (DDNode r) = DDNode $ unsafePerformIO $ 
 	withForeignPtr l $ \lp -> 
	withForeignPtr r $ \rp -> do
	node <- f m lp rp
	newForeignPtrEnv deref m node

cuddArg3 :: (Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CDDNode -> IO (Ptr CDDNode)) -> DDManager -> DDNode -> DDNode-> DDNode -> DDNode
cuddArg3 f (DDManager m) (DDNode l) (DDNode r) (DDNode x) = DDNode $ unsafePerformIO $ 
 	withForeignPtr l $ \lp -> 
	withForeignPtr r $ \rp -> 
	withForeignPtr x $ \xp -> do
	node <- f m lp rp xp
	newForeignPtrEnv deref m node

readOne :: DDManager -> DDNode
readOne = cuddArg0 c_cuddReadOneWithRef

readLogicZero :: DDManager -> DDNode
readLogicZero  = cuddArg0 c_cuddReadLogicZeroWithRef

ithVar :: DDManager -> Int -> DDNode
ithVar m i = cuddArg0 (flip c_cuddBddIthVar (fromIntegral i)) m

bAnd :: DDManager -> DDNode -> DDNode -> DDNode
bAnd = cuddArg2 c_cuddBddAnd

bOr :: DDManager -> DDNode -> DDNode -> DDNode
bOr = cuddArg2 c_cuddBddOr

bNand :: DDManager -> DDNode -> DDNode -> DDNode
bNand = cuddArg2 c_cuddBddNand

bNor :: DDManager -> DDNode -> DDNode -> DDNode
bNor = cuddArg2 c_cuddBddNor

bXor :: DDManager -> DDNode -> DDNode -> DDNode
bXor = cuddArg2 c_cuddBddXor

bXnor :: DDManager -> DDNode -> DDNode -> DDNode
bXnor = cuddArg2 c_cuddBddXnor

bNot :: DDManager -> DDNode -> DDNode
bNot = cuddArg1 (const c_cuddNot)

dumpDot' :: DDManager -> [DDNode] -> Maybe [String] -> Maybe [String] -> Ptr CFile -> IO Int
dumpDot' (DDManager m) nodes iNames oNames file = liftM fromIntegral $
    withForeignArrayPtrLen (map unDDNode nodes) $ \nn np -> 
        maybe ($ nullPtr) withStringArrayPtr iNames $ \iNamesP -> 
            maybe ($ nullPtr) withStringArrayPtr oNames $ \oNamesP -> do
                c_cuddDumpDot m (fromIntegral nn) np iNamesP oNamesP file

foreign import ccall safe "fopen" 
    c_fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall safe "fclose"
    c_fclose :: (Ptr CFile) -> IO ()

dumpDot :: DDManager -> DDNode -> String -> IO Int
dumpDot m n fName = 
    withCAString fName $ \fNameP -> 
        withCAString "w" $ \md -> do
            file <- c_fopen fNameP md
            res <- dumpDot' m [n] Nothing Nothing file
            c_fclose file
            return res

eval :: DDManager -> DDNode -> [Int] -> Bool
eval (DDManager m) (DDNode n) a = unsafePerformIO $ do
    res <- withArray (map fromIntegral a) $ \ap -> 
        withForeignPtr n $ \np -> 
            c_cuddEval m np ap
    return $ (==0) $ c_cuddIsComplement res

printMinterm :: DDManager -> DDNode -> IO ()
printMinterm (DDManager m) (DDNode n) = 
    withForeignPtr n $ c_cuddPrintMinterm m 

foreign import ccall safe "allSat"
    c_allSat :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> Ptr CInt -> IO (Ptr (Ptr CInt))

foreign import ccall safe "oneSat"
    c_oneSat :: Ptr CDDManager -> Ptr CDDNode -> Ptr CInt -> IO (Ptr CInt)

allSat :: DDManager -> DDNode -> [[SatBit]]
allSat (DDManager m) (DDNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    alloca $ \ntermsptr -> 
    withForeignPtr n $ \np -> do
    res <- c_allSat m np ntermsptr nvarsptr
    nterms <- liftM fromIntegral $ peek ntermsptr
    res <- peekArray nterms res
    nvars <- liftM fromIntegral $ peek nvarsptr
    res <- mapM (peekArray nvars) res
    return $ map (map (toSatBit . fromIntegral)) res

oneSat :: DDManager -> DDNode -> Maybe [SatBit]
oneSat (DDManager m) (DDNode n) = unsafePerformIO $ 
    alloca $ \nvarsptr ->
    withForeignPtr n $ \np -> do
    res <- c_oneSat m np nvarsptr
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map (toSatBit . fromIntegral) res

foreign import ccall safe "onePrime"
    c_onePrime :: Ptr CDDManager -> Ptr CDDNode -> Ptr CDDNode -> Ptr CInt -> IO (Ptr CInt)

onePrime :: DDManager -> DDNode -> DDNode -> Maybe [Int]
onePrime (DDManager m) (DDNode l) (DDNode u) = unsafePerformIO $ 
    alloca $ \nvarsptr -> 
    withForeignPtr l $ \lp -> 
    withForeignPtr u $ \up -> do
    res <- c_onePrime m lp up nvarsptr 
    if res==nullPtr then return Nothing else do
        nvars <- liftM fromIntegral $ peek nvarsptr
        res <- peekArray nvars res
        return $ Just $ map fromIntegral res

readSize :: DDManager -> Int
readSize (DDManager m) = fromIntegral $ unsafePerformIO $ c_cuddReadSize m

supportIndex :: DDManager -> DDNode -> [Bool]
supportIndex (DDManager m) (DDNode n) = unsafePerformIO $ 
	withForeignPtr n $ \np -> do
    res <- c_cuddSupportIndex m np
    size <- c_cuddReadSize m
    res <- peekArray (fromIntegral size) res
    return $ map toBool res

bExists :: DDManager -> DDNode -> DDNode -> DDNode
bExists = cuddArg2 c_cuddBddExistAbstract

bForall :: DDManager -> DDNode -> DDNode -> DDNode
bForall = cuddArg2 c_cuddBddUnivAbstract

bIte :: DDManager -> DDNode -> DDNode -> DDNode -> DDNode
bIte = cuddArg3 c_cuddBddIte

swapVariables :: DDManager -> DDNode -> [DDNode] -> [DDNode] -> DDNode
swapVariables (DDManager m) (DDNode d) s1 s2 = DDNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withForeignArrayPtrLen (map unDDNode s1) $ \s1 s1ps -> 
    withForeignArrayPtrLen (map unDDNode s2) $ \s2 s2ps -> do
    when (s1 /= s2) (error "cuddBddSwapVariables: variable lists have different sizes")
    node <- c_cuddBddSwapVariables m dp s1ps s2ps (fromIntegral s1)
    newForeignPtrEnv deref m node

permute :: DDManager -> DDNode -> [Int] -> DDNode 
permute (DDManager m) (DDNode d) indexes = DDNode $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    withArray (map fromIntegral indexes) $ \ip -> do
    node <- c_cuddBddPermute m dp ip 
    newForeignPtrEnv deref m node

xGtY :: DDManager -> [DDNode] -> [DDNode] -> DDNode
xGtY (DDManager m) x y = DDNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDDNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDDNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXgty: variable lists have different sizes")
    node <- c_cuddXgty m (fromIntegral xl) nullPtr xp yp
    newForeignPtrEnv deref m node

xEqY :: DDManager -> [DDNode] -> [DDNode] -> DDNode
xEqY (DDManager m) x y = DDNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDDNode x) $ \xl xp -> 
    withForeignArrayPtrLen (map unDDNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXeqy: variable lists have different sizes")
    node <- c_cuddXeqy m (fromIntegral xl) xp yp
    newForeignPtrEnv deref m node

inequality :: DDManager -> Int -> Int -> [DDNode] -> [DDNode] -> DDNode
inequality (DDManager m) n c x y = DDNode $ unsafePerformIO $ 
    withForeignArrayPtr (map unDDNode x) $ \xp -> 
    withForeignArrayPtr (map unDDNode y) $ \yp -> do
    node <- c_cuddInequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

disequality :: DDManager -> Int -> Int -> [DDNode] -> [DDNode] -> DDNode
disequality (DDManager m) n c x y = DDNode $ unsafePerformIO $
    withForeignArrayPtr (map unDDNode x) $ \xp -> 
    withForeignArrayPtr (map unDDNode y) $ \yp -> do
    node <- c_cuddDisequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

interval :: DDManager -> [DDNode] -> Int -> Int -> DDNode
interval (DDManager m) vararr lower upper =  DDNode $ unsafePerformIO $ 
    withForeignArrayPtrLen (map unDDNode vararr) $ \sz vp -> do
    node <- c_cuddBddInterval m (fromIntegral sz) vp (fromIntegral lower) (fromIntegral upper)
    newForeignPtrEnv deref m node

nodeReadIndex :: DDNode -> Int
nodeReadIndex (DDNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddNodeReadIndex 

dagSize :: DDNode -> Int
dagSize (DDNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddDagSize 

indicesToCube :: DDManager -> [Int] -> DDNode
indicesToCube (DDManager m) indices = DDNode $ unsafePerformIO $ 
    withArrayLen (map fromIntegral indices) $ \size ip -> do
    node <- c_cuddIndicesToCube m ip (fromIntegral size)
    newForeignPtrEnv deref m node

liCompaction :: DDManager -> DDNode -> DDNode -> DDNode
liCompaction = cuddArg2 c_cuddBddLICompaction

minimize :: DDManager -> DDNode -> DDNode -> DDNode
minimize = cuddArg2 c_cuddBddMinimize

pickOneMinterm :: DDManager -> DDNode -> [DDNode] -> Maybe DDNode
pickOneMinterm (DDManager m) (DDNode d) vars = unsafePerformIO $
	withForeignPtr d $ \dp -> 
	withForeignArrayPtrLen (map unDDNode vars) $ \vs vp -> do
	node <- c_cuddBddPickOneMinterm m dp vp (fromIntegral vs)
	if node == nullPtr then return Nothing else do
		nd <- newForeignPtrEnv deref m node
		return $ Just $ DDNode nd

printInfo :: DDManager -> Ptr CFile -> IO Int
printInfo (DDManager m) cf = liftM fromIntegral $ c_cuddPrintInfo m cf

readPerm :: DDManager -> Int -> Int
readPerm (DDManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadPerm m (fromIntegral i)

readInvPerm :: DDManager -> Int -> Int
readInvPerm (DDManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadInvPerm m (fromIntegral i)

readPerms :: DDManager -> [Int]
readPerms m = map (readPerm m) [0..(readSize m - 1)]

readInvPerms :: DDManager -> [Int]
readInvPerms m = map (readInvPerm m) [0..(readSize m -1)]

readTree :: DDManager -> IO (Ptr CMtrNode) 
readTree (DDManager m) = c_cuddReadTree m

countLeaves :: DDNode -> Int
countLeaves (DDNode d) = fromIntegral $ unsafePerformIO $ 
    withForeignPtr d $ \dp -> 
    c_cuddCountLeaves dp

countMinterm :: DDManager -> DDNode -> Int -> Double
countMinterm (DDManager m) (DDNode d) n = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountMinterm m dp (fromIntegral n) 

countPathsToNonZero :: DDNode -> Double
countPathsToNonZero (DDNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountPathsToNonZero dp

countPath :: DDNode -> Double
countPath (DDNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp -> 
    c_cuddCountPath dp

printDebug :: DDManager -> DDNode -> Int -> Int -> IO Int
printDebug (DDManager m) (DDNode d) n pr = liftM fromIntegral $ 
    withForeignPtr d $ \dp -> 
    c_cuddPrintDebug m dp (fromIntegral n) (fromIntegral pr)

andAbstract :: DDManager -> DDNode -> DDNode -> DDNode -> DDNode  
andAbstract = cuddArg3 c_cuddBddAndAbstract

xorExistAbstract :: DDManager -> DDNode -> DDNode -> DDNode -> DDNode  
xorExistAbstract = cuddArg3 c_cuddBddXorExistAbstract

transfer :: DDManager -> DDManager -> DDNode -> DDNode
transfer (DDManager m1) (DDManager m2) (DDNode x) = DDNode $ unsafePerformIO $ 
    withForeignPtr x $ \xp -> do
        node <- c_cuddBddTransfer m1 m2 xp
        newForeignPtrEnv deref m2 node

makePrime :: DDManager -> DDNode -> DDNode -> DDNode
makePrime = cuddArg2 c_cuddBddMakePrime

constrain :: DDManager -> DDNode -> DDNode -> DDNode
constrain = cuddArg2 c_cuddBddConstrain

restrict :: DDManager -> DDNode -> DDNode -> DDNode
restrict = cuddArg2 c_cuddBddRestrict

squeeze :: DDManager -> DDNode -> DDNode -> DDNode
squeeze = cuddArg2 c_cuddBddSqueeze

largestCube :: DDManager -> DDNode -> (Int, DDNode)
largestCube (DDManager m) (DDNode n) = unsafePerformIO $ 
    alloca $ \lp ->
    withForeignPtr n $ \np -> do
    node <- c_cuddLargestCube m np lp
    res <- newForeignPtrEnv deref m node
    l <- peek lp
    return (fromIntegral l, DDNode res)
     
lEq :: DDManager -> DDNode -> DDNode -> Bool
lEq (DDManager m) (DDNode l) (DDNode r) = (==1) $ unsafePerformIO $ 
    withForeignPtr l $ \lp -> 
    withForeignPtr r $ \rp ->
    c_cuddBddLeq m lp rp

ddNodeToInt :: Integral i => DDNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDDNode 

