-- |
{-# LANGUAGE TypeSynonymInstances #-}
module Point where
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable

data Point3D = P {-# UNPACK #-} !CDouble
                 {-# UNPACK #-} !CDouble
                 {-# UNPACK #-} !CDouble
                   deriving (Show)

instance Storable Point3D where
  sizeOf _ = sizeOf (undefined :: CDouble) * 3
  alignment _ = alignment (undefined :: CDouble)

  {-# INLINE peek #-}
  peek p = do
    x <- peekElemOff q 0
    y <- peekElemOff q 1
    z <- peekElemOff q 2
    return (P x y z)
    where
      q = castPtr p

  {-# INLINE poke #-}
  poke p (P x y z) = do
    pokeElemOff q 0 x
    pokeElemOff q 1 y
    pokeElemOff q 2 z
    where
      q = castPtr p

type Point = Point3D
type Normal = Point3D

_x :: Point -> Double
_x (P x _ _) = realToFrac x

_y :: Point -> Double
_y (P _ y _) = realToFrac y

_z :: Point -> Double
_z (P _ _ z) = realToFrac z

instance Num Point where
  (P x y z) + (P x' y' z') = P (x + x') (y + y') (z + z')
  (P x y z) - (P x' y' z') = P (x - x') (y - y') (z - z')
  (P x y z) * (P x' y' z') = P (x * x') (y * y') (z * z')
  negate p = zeroP - p
  abs (P x y z) = P (abs x) (abs y) (abs z)
  signum = undefined
  fromInteger = undefined

zeroP :: Point
zeroP = point 0 0 0

dot :: Point -> Point -> Double
dot (P a b c) (P a' b' c') =  realToFrac $ a*a' + b*b' + c*c'

norm :: Point -> Double
norm (P a b c) = realToFrac $ sqrt(a^2 + b^2 + c^2)

distance :: Point -> Point -> Double
distance (P x y z) (P x' y' z') = realToFrac $ sqrt((x - x')^2 + (y - y')^2 + (z - z')^2)

(.*) :: Double -> Point -> Point
(.*) k (P x y z) = point' (x*k') (y*k') (z*k')
  where
    k' = realToFrac k

point :: Double -> Double -> Double -> Point
point x y z = P x' y' z'
  where
    x' = realToFrac x
    y' = realToFrac y
    z' = realToFrac z

point' :: CDouble -> CDouble -> CDouble -> Point
point' = P

angle :: Point -> Point -> Point -> Double
angle x y z = acos(prod / (d1*d2))
  where
    v1 = x - y
    v2 = z - y
    d1 = norm v1
    d2 = norm v2
    prod = dot v1 v2

makePoint :: [Double] -> Point
makePoint (x:y:z:[]) = point x y z
makePoint _ = error "Wrong point number!"
