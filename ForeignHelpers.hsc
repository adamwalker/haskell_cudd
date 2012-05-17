module ForeignHelpers (
    withForeignArray,
    withForeignArrayPtr,
    withForeignArrayPtrLen
    ) where

import Foreign

withForeignArray :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignArray [] func = func []
withForeignArray (p:ptrs) func = withForeignPtr p $ \ptr -> 
    withForeignArray ptrs $ \x -> func (ptr:x)

withForeignArrayPtr :: [ForeignPtr a] -> (Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtr fps func = withForeignArray fps $ \ap -> withArray ap func

withForeignArrayPtrLen :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withForeignArrayPtrLen fps func = withForeignArray fps $ \ap -> withArrayLen ap func
