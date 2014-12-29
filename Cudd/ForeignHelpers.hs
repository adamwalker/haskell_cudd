module Cudd.ForeignHelpers (
    withForeignArray,
    withForeignArrayPtr,
    withForeignArrayPtrLen,
    withStringArray,
    withStringArrayPtr
    ) where

import Foreign
import Foreign.C.String
import Foreign.ForeignPtr.Unsafe

withForeignArray :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignArray [] func = func []
withForeignArray (p:ptrs) func = withForeignPtr p $ \ptr -> 
    withForeignArray ptrs $ \x -> func (ptr:x)

withForeignArray' :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignArray' ptrs io = do
    r <- io (map unsafeForeignPtrToPtr ptrs)
    mapM touchForeignPtr ptrs
    return r

withForeignArrayPtr :: [ForeignPtr a] -> (Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtr fps func = withForeignArray fps $ \ap -> withArray ap func

withForeignArrayPtrLen :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtrLen fps func = withForeignArray fps $ \ap -> withArrayLen ap func

withStringArray :: [String] -> ([CString] -> IO a) -> IO a
withStringArray [] func = func []
withStringArray (s:strings) func = withCString s $ \ptr -> 
    withStringArray strings $ \x -> func (ptr : x)

withStringArrayPtr :: [String] -> (Ptr CString -> IO a) -> IO a
withStringArrayPtr strings func = withStringArray strings $ \sp -> withArray sp func

