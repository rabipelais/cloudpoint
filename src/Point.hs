-- | 

module Point where

data Point = P !Double !Double !Double deriving (Show, Eq)

instance Num Point where
  (P x y z) + (P x' y' z') = P (x + x') (y + y') (z + z')
  (P x y z) - (P x' y' z') = P (x - x') (y - y') (z - z')
  (P x y z) * (P x' y' z') = P (x * x') (y * y') (z * z')
  negate p = (P 0 0 0) - p
  abs (P x y z) = P (abs x) (abs y) (abs z) 
  signum = undefined
  fromInteger = undefined

distance :: Point -> Point -> Double
distance (P x y z) (P x' y' z') = sqrt((x - x')^2 + (y - y')^2 + (z - z')^2)

angle :: Point -> Point -> Point -> Double
angle (P x y z) (P x' y' z') (P x'' y'' z'') = acos (prod / (d1*d2))
  where 
    v1 = (x - x', y - y', z - z')
    v2 = (x'' - x', y'' - y', z'' - z')
    norm (a, b, c) = sqrt(a^2 + b^2 + c^2)
    d1 = norm v1
    d2 = norm v2
    dot (a, b, c) (a', b', c') = a*a' + b*b' + c*c'
    prod = dot v1 v2
makePoint :: [Double] -> Point
makePoint (x:y:z:[]) = P x y z
makePoint _ = error "Wrong point number!"
