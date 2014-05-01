-- | 

module Point where

data Point = P !Double !Double !Double deriving (Show, Eq)

_x :: Point -> Double
_x (P x _ _) = x

_y :: Point -> Double
_y (P _ y _) = y

_z :: Point -> Double
_z (P _ _ z) = z

instance Num Point where
  (P x y z) + (P x' y' z') = P (x + x') (y + y') (z + z')
  (P x y z) - (P x' y' z') = P (x - x') (y - y') (z - z')
  (P x y z) * (P x' y' z') = P (x * x') (y * y') (z * z')
  negate p = (P 0 0 0) - p
  abs (P x y z) = P (abs x) (abs y) (abs z) 
  signum = undefined
  fromInteger = undefined

dot :: Point -> Point -> Double
dot (P a b c) (P a' b' c') =  a*a' + b*b' + c*c'

norm :: Point -> Double
norm (P a b c) = sqrt(a^2 + b^2 + c^2)

distance :: Point -> Point -> Double
distance (P x y z) (P x' y' z') = sqrt((x - x')^2 + (y - y')^2 + (z - z')^2)

(.*) :: Double -> Point -> Point
(.*) k (P x y z) = point (x*k) (y*k) (z*k)

point :: Double -> Double -> Double -> Point
point x y z = (P x y z)

angle :: Point -> Point -> Point -> Double
angle x y z = acos (prod / (d1*d2))
  where 
    v1 = x - y
    v2 = z - y
    d1 = norm v1
    d2 = norm v2
    prod = dot v1 v2
    
makePoint :: [Double] -> Point
makePoint (x:y:z:[]) = P x y z
makePoint _ = error "Wrong point number!"
