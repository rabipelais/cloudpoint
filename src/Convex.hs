-- |

module Convex where

import           Control.Monad
import           Data.Vector.Storable
import qualified Data.Vector.Storable  as SV
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe

import           Point

foreign import ccall "chull.h convexhull"
  c_convexhull :: Ptr Point -> CInt -> Ptr Point -> Ptr CInt -> IO ()

convexHull :: Vector Point -> Vector Point
convexHull points =
  unsafePerformIO $
  unsafeWith points $  \cpoints ->
  allocaArray n $ \coutput ->
  alloca $ \cres ->
  do c_convexhull cpoints (fromIntegral n) coutput cres
     resNum <- peek cres
     res <- peekArray (fromIntegral resNum) coutput
     return $ fromList res
  where
    n = SV.length points
