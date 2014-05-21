{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Point where

import           Data.Vector
import           Prelude     hiding (foldr1, map, zipWith)

type Point = Vector Double

_x :: Point -> Double
_x p = p ! 0

_y :: Point -> Double
_y p = p ! 1

_z :: Point -> Double
_z p = p ! 2

zeroP :: Point
zeroP = point 0 0 0

point :: Double -> Double -> Double -> Point
point x y z = fromList [x, y, z]

instance Num Point where
  p + p' = zipWith (+) p p'
  p - p' = zipWith (-) p p'
  p * p' = zipWith (*) p p'
  negate p = zeroP - p
  abs = map abs
  signum = undefined
  fromInteger = undefined

dot :: Point -> Point -> Double
dot p p' =  foldr1 (+) $ p * p'

norm :: Point -> Double
norm p = sqrt(dot p p)

distance :: Point -> Point -> Double
distance p p' = norm (p - p')

(.*) :: Double -> Point -> Point
(.*) k = map (k *)


angle :: Point -> Point -> Point -> Double
angle x y z = acos (prod / (d1*d2))
  where
    v1 = x - y
    v2 = z - y
    d1 = norm v1
    d2 = norm v2
    prod = dot v1 v2

makePoint :: [Double] -> Point
makePoint (x:y:z:[]) = point x y z
makePoint _ = error "Wrong point number!"
