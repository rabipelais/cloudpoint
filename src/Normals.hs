-- | 

module Normals where

import           Prelude hiding (foldr1, head, tail, filter, (++), take, null, length, map, zipWith, zip)
import           Data.Vector
import  Numeric.LinearAlgebra hiding (Vector, toList, fromList)

import           Point

type Normal = Vector Double
type CovMat = Vector Double

normals :: Int -> Vector Point -> Vector Normal
normals k ps = map (\p -> normalAt p k ps) ps

normalAt :: Point -> Int -> Vector Point -> Normal
normalAt p k ps = firstEigenVector $ covarianceMatrix center ps
  where 
    center = centroid $ knearest k p ps

knearest :: Int -> Point -> Vector Point -> Vector Point
knearest k p  = take k . qsort
  where
    qsort ps
      | (null ps) = empty
      | (head ps == p) = qsort (tail ps)
      | otherwise = qsort left ++ (singleton pivot) ++ qsort right
      where 
        pivot = head ps
        dp = distance p pivot
        left = filter (\x -> distance x p < dp) ps
        right = filter (\x -> dp <= distance x p) ps

centroid :: Vector Point -> Point
centroid ps = (P (x/k) (y/k) (z/k))
  where 
    (P x y z) =  foldr1 (+) ps
    k = fromIntegral $ length ps

covarianceMatrix :: Point -> Vector Point -> Vector Double
covarianceMatrix center ps = mat
  where 
    ps' = map ((-) center) ps
    n = length ps
    mat = map (/ fromIntegral n) $ foldr1 (zipWith (+)) $ map outerProd ps'

outerProd :: Point -> Vector Double
outerProd (P x y z) = vec
  where 
    xx = x*x
    xy = x*y
    xz = x*z
    yz = y*z
    yy = y*y
    zz = z*z
    vec = fromList [ xx, xy, xz
                      , xy, yy, yz
                      , xz, yz, zz]

firstEigenVector :: Vector Double -> Vector Double
firstEigenVector m =  convert firstvec
  where
    (evals, evecs) = eigSH' ((3><3) $ toList m)
    evecs' = fromList $ toColumns evecs
    firstvec = snd $ head $ filter (\(s, _) -> s > 0) (zip (convert evals) evecs')
