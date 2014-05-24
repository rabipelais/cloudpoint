-- |

module Convex where

import           Data.Vector.Storable
import qualified Data.Vector.Storable  as SV
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe

import           Point

data CFacet = CFacet {-# UNPACK #-} !CInt
                     {-# UNPACK #-} !CInt
                     {-# UNPACK #-} !CInt
                       deriving (Show)

instance Storable CFacet where
  sizeOf _ = sizeOf (undefined :: CInt) * 3
  alignment _ = alignment (undefined :: CInt)

  {-# INLINE peek #-}
  peek p = do
    x <- peekElemOff q 0
    y <- peekElemOff q 1
    z <- peekElemOff q 2
    return (CFacet x y z)
    where
      q = castPtr p

  {-# INLINE poke #-}
  poke p (CFacet x y z) = do
    pokeElemOff q 0 x
    pokeElemOff q 1 y
    pokeElemOff q 2 z
    where
      q = castPtr p


foreign import ccall "chull.h convexhull"
  c_convexhull :: Ptr Point -- input points
                  -> CInt   -- number of points
                  -> Ptr Point -- points on the convex hull
                  -> Ptr CInt  -- number of points on the convex hull
                  -> Ptr CFacet -- Facet indices
                  -> Ptr CInt -- number of facets
                  -> Ptr Double -- area
                  -> Ptr Double -- volume
                  -> IO ()

data ConvexHull = ConvexHull { _points :: Vector Point
                             , _facets :: Vector CFacet
                             , _area   :: Double
                             , _volume :: Double}
                             deriving (Show)

convexHull :: Vector Point -> ConvexHull
convexHull input  =
  unsafePerformIO $
  unsafeWith input $  \cinput ->
  allocaArray n $ \coutput ->
  alloca $ \cnumhull ->
  allocaArray (n * 3) $ \cindices ->
  alloca $ \cnumidx ->
  alloca $ \carea ->
  alloca $ \cvolume ->
  do c_convexhull cinput (fromIntegral n) coutput cnumhull cindices cnumidx carea cvolume
     numhull <- peek cnumhull
     hullPoints <- peekArray (fromIntegral numhull) coutput
     numidx <- peek cnumidx
     hullIdx <- peekArray (fromIntegral numidx) cindices
     area <- peek carea
     volume <- peek cvolume
     return ConvexHull { _points = fromList hullPoints
                         , _facets = fromList hullIdx
                         , _area = area
                         , _volume = volume}
  where
    n = SV.length input
