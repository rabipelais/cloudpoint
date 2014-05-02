-- | 

module Generate where

import System.Random
import Point

sampleCube :: Int ->  [Point]
sampleCube samples = points
  where
    front = take (samples `quot` 6) $ map (\(a, b) -> point (a - 0.5) (b - 0.5) (1/2)) (randomListPairs (mkStdGen 1))
    back = take (samples `quot` 6) $  map (\(a, b) -> point (a - 0.5) (b - 0.5) (-1/2)) (randomListPairs (mkStdGen 2))
    top = take (samples `quot` 6) $  map (\(a, b) -> point (1/2) (a - 0.5) (b - 0.5)) (randomListPairs (mkStdGen 3))
    bottom = take (samples `quot` 6) $  map (\(a, b) -> point (-1/2) (a - 0.5) (b - 0.5)) (randomListPairs (mkStdGen 4))
    left = take (samples `quot` 6) $  map (\(a, b) -> point (a - 0.5) (-1/2) (b - 0.5)) (randomListPairs (mkStdGen 5))
    right = take (samples `quot` 6) $  map (\(a, b) -> point (a - 0.5) (1/2) (b - 0.5)) (randomListPairs (mkStdGen 6))
    points = concat [front, back, top, bottom, left, right]


randomListPairs g = l
  where 
    (g', g'') = split g
    l = zip (randoms g') (randoms g'')

projectSphere2Cube :: Point -> Point
projectSphere2Cube p = p'
  where 
    max' = max (max (abs $ _x p) (abs $ _y p)) (abs $ _z p)
    scale = 1 / max'
    p' = scale .* p

sampleSphere :: Int -> StdGen -> StdGen -> [Point]
sampleSphere samples g g' = points
  where 
    us = take samples (randoms g :: [Double])
    vs = take samples (randoms g' :: [Double])
    thetas = map (\u -> 2 * pi * u) us
    phis = map (\v -> acos (2*v - 1)) vs
    points = zipWith go thetas phis
    go theta phi = point (cos theta * sin phi) (sin theta * sin phi) (cos phi)

sampleTetrahedron :: Int -> StdGen -> StdGen -> StdGen -> [Point]
sampleTetrahedron samples g g' g'' = take samples points
  where 
    as = (randoms g :: [Double])
    bs = (randoms g' :: [Double])
    cs = (randoms g'' :: [Double])
    v1 = point 1 0 (-1/sqrt(2))
    v2 = point (-1) 0 (-1/sqrt(2))
    v3 = point 0 1 (1/sqrt(2))
    v4 = point 0 (-1) (1/sqrt(2))
    points = map (build . project . coords) $ filter (\(a, b, c) -> a + b + c <= 1.0) $ zip3 as bs cs
    coords (a, b, c) = (a, b, c, 1 - a - b - c)
    build (a, b, c, d) = (a .* v1) + (b .* v2) + (c .* v3) + (d .* v4)
    project (a, b, c, d) 
      | m == a = (0, b + a/3, c + a/3, d + a/3)
      | m == b = (a + b/3, 0, c + b/3, d + b/3)
      | m == c = (a + c/3, b + c/3, 0, d + c/3)
      | otherwise = (a + d/3, b + d/3, c + d/3, 0)
      where m = min (min a b) (min c d)
      
sampleTetrahedronFull :: Int -> StdGen -> StdGen -> StdGen -> [Point]
sampleTetrahedronFull samples g g' g'' = take samples points
  where 
    as = (randoms g :: [Double])
    bs = (randoms g' :: [Double])
    cs = (randoms g'' :: [Double])
    v1 = point 1 0 (-1/sqrt(2))
    v2 = point (-1) 0 (-1/sqrt(2))
    v3 = point 0 1 (1/sqrt(2))
    v4 = point 0 (-1) (1/sqrt(2))
    points = map (build . coords) $ filter (\(a, b, c) -> a + b + c <= 1.0) $ zip3 as bs cs
    coords (a, b, c) = (a, b, c, 1 - a - b - c)
    build (a, b, c, d) = (a .* v1) + (b .* v2) + (c .* v3) + (d .* v4)
