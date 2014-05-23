-- |

module Normals where

import qualified Data.List             as L
import           Data.Ord              (comparing)
import qualified Data.Vector           as V
import           Data.Vector.Storable
import           Numeric.LinearAlgebra hiding (Vector, fromList, toList)
import           Prelude               hiding (filter, foldr1, head, last,
                                        length, map, null, reverse, tail, take,
                                        zip, zipWith, (++))

import           Point

type CovMat = V.Vector Double


-- | @k@ is the parameter for the k-NN estimator
normals' :: Int -> Vector Point -> Vector Normal
normals' k ps = map (\p -> normalAt p k ps) ps

normals :: Int -> Vector Point -> Vector Normal
normals k ps = zipWith go ps ns
  where
    ns = normals' k ps
    go = orientNormalToViewpoint zeroP

normalsView :: Int -> Point -> Vector Point -> Vector Normal
normalsView k v ps = zipWith go ps ns
  where
    ns = normals' k ps
    go = orientNormalToViewpoint v

normalAt :: Point -> Int -> Vector Point -> Normal
normalAt p k ps = packVector $ firstEigenVector $ covarianceMatrix center kNN
  where
    kNN = knearest k p ps
    center = centroid kNN

orientNormalToViewpoint :: Point -> Point -> Normal -> Normal
orientNormalToViewpoint v p n = if Point.dot n (v - p) > 0
                                 then n
                                 else (-1) .* n

knearest :: Int -> Point -> Vector Point -> Vector Point
knearest k p ps = fromList $ L.take k $ L.sortBy (comparing (distance p)) (toList ps)

centroid :: Vector Point -> Point
centroid ps = k .* p
  where
    p = foldr1 (+) ps
    k = 1.0 / fromIntegral (length ps)

covarianceMatrix :: Point -> Vector Point -> CovMat
covarianceMatrix center ps = mat
  where
    ps' = map (center -) ps
    n = length ps
    mat = V.map (/ fromIntegral n) $ V.foldr1 (V.zipWith (+)) $ V.map outerProd $ convert ps'

outerProd :: Point -> V.Vector Double
outerProd p = vec
  where
    x = _x p
    y = _y p
    z = _z p
    xx = x*x
    xy = x*y
    xz = x*z
    yz = y*z
    yy = y*y
    zz = z*z
    vec = V.fromList [ xx, xy, xz
                     , xy, yy, yz
                     , xz, yz, zz]

packVector :: Vector Double -> Normal
packVector v = point (v!0) (v!1) (v!2)

-- | From larger to smaller
eigenValsAndEigenVecs :: CovMat -> (Vector Double, Matrix Double)
eigenValsAndEigenVecs m = (eval, evecs)
  where
    (val, evecs) = eigSH' ((3><3) $ V.toList m)
    eval = convert val

firstEigenVector :: CovMat -> Vector Double
firstEigenVector m =  convert firstvec
  where
    (evals, evecs) = eigenValsAndEigenVecs m
    evecs' = V.fromList $ toColumns evecs
    firstvec = snd $ V.last $ V.filter (\(s, _) -> s >= 0) (V.zip (convert evals) evecs')
