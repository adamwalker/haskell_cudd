module CuddExplicitDeref (
    bzero,
    bone,
    bvar,
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
    mapVars,
    DDNode,
    STDdManager,
    leq,
    CuddExplicitDeref.shift,
    ref,
    largestCube,
    makePrime,
    supportIndices,
    indicesToCube,
    computeCube,
    nodesToCube,
    readSize,
    satCube,
    compose,
    andAbstract,
    xorExistAbstract,
    leqUnless,
    equivDC,
    xeqy,
    debugCheck,
    checkKeys
    ) where

import Foreign hiding (void)
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.ST.Lazy
import Control.Monad

import CuddC
import CuddInternal hiding (deref)

newtype DDNode s u = DDNode {unDDNode :: Ptr CDdNode} deriving (Ord, Eq, Show)

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

bnot (DDNode x) = DDNode $ unsafePerformIO $ c_cuddNot x
bvar (STDdManager m) i = liftM DDNode $ unsafeIOToST $ c_cuddBddIthVar m (fromIntegral i)

deref :: STDdManager s u -> DDNode s u -> ST s ()
deref (STDdManager m) (DDNode x) = unsafeIOToST $ c_cuddIterDerefBdd m x

setVarMap :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> ST s ()
setVarMap (STDdManager m) xs ys = unsafeIOToST $ 
    withArrayLen (map unDDNode xs) $ \xl xp -> 
    withArrayLen (map unDDNode ys) $ \yl yp -> do
    when (xl /= yl) (error "setVarMap: lengths not equal")
    void $ c_cuddSetVarMap m xp yp (fromIntegral xl)

mapVars :: STDdManager s u -> DDNode s u -> ST s (DDNode s u)
mapVars (STDdManager m) (DDNode x) = liftM DDNode $ unsafeIOToST $ c_cuddBddVarMap m x

leq :: STDdManager s u -> DDNode s u -> DDNode s u -> ST s Bool
leq (STDdManager m) (DDNode x) (DDNode y) = liftM (==1) $ unsafeIOToST $ c_cuddBddLeq m x y

shift :: STDdManager s u -> [DDNode s u] -> [DDNode s u] -> DDNode s u -> ST s (DDNode s u)
shift (STDdManager m) nodesx nodesy (DDNode x) = unsafeIOToST $ 
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

satCube :: STDdManager s u -> DDNode s u -> ST s [Int]
satCube ma@(STDdManager m) (DDNode x) = unsafeIOToST $ do
    size <- liftM fromIntegral $ c_cuddReadSize m
    allocaArray size $ \resptr -> do
    c_cuddBddToCubeArray m x resptr
    res <- peekArray size resptr
    return $ map fromIntegral res

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

debugCheck :: STDdManager s u -> ST s ()
debugCheck (STDdManager m) = unsafeIOToST $ c_cuddDebugCheck m

checkKeys :: STDdManager s u -> ST s ()
checkKeys (STDdManager m) = unsafeIOToST $ c_cuddCheckKeys m

{-
refCount :: STDdManager s u -> STDdNode s u -> ST s (DdNode s u)

unRefCount :: STDdManager s u -> DdNode s u -> ST s (STDdNode s u)
-}
