-- |

module Normals where

import qualified Data.List             as L
import           Data.Ord              (comparing)
import           Data.Vector
import           Numeric.LinearAlgebra hiding (Vector, fromList, toList)
import           Prelude               hiding (filter, foldr1, head, last,
                                        length, map, null, reverse, tail, take,
                                        zip, zipWith, (++))

import           Point

type Normal = Vector Double
type CovMat = Vector Double

--  ( src/Point.hs, dist/build/Point.o )
-- | @k@ is the parameter for the k-NN estimator
normals :: Int -> Vector Point -> Vector Normal
normals k ps = map (\p -> normalAt p k ps) ps

orientedNormalsOrigin :: Int -> Vector Point -> Vector Normal
orientedNormalsOrigin k ps = zipWith go ps ns
  where
    ns = normals k ps
    go = orientNormalToViewpoint zeroP


normalAt :: Point -> Int -> Vector Point -> Normal
normalAt p k ps = firstEigenVector $ covarianceMatrix center kNN
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
    k = 1.0 / (fromIntegral $ length ps)

covarianceMatrix :: Point -> Vector Point -> Vector Double
covarianceMatrix center ps = mat
  where
    ps' = map ((-) center) ps
    n = length ps
    mat = map (/ fromIntegral n) $ foldr1 (zipWith (+)) $ map outerProd ps'

outerProd :: Point -> Vector Double
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
    vec = fromList [ xx, xy, xz
                   , xy, yy, yz
                   , xz, yz, zz]

-- | From larger to smaller
eigenValsAndEigenVecs m = eigSH' ((3><3) $ toList m)

firstEigenVector :: Vector Double -> Vector Double
firstEigenVector m =  convert firstvec
  where
    (evals, evecs) = eigenValsAndEigenVecs m
    evecs' = fromList $ toColumns evecs
    firstvec = snd $ last $ filter (\(s, _) -> s >= 0) (zip (convert evals) evecs')
