module Sampling  where

import           Data.List
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as M
import qualified Data.Vector.Storable as SV
import           System.Random

import           Point


maxSamples :: Int
maxSamples = 100000

maxPairs :: Int
maxPairs = 100000
maxTrios :: Int
maxTrios = 100000

-- | @bins@ is the number of bins and @width@ the size of each bin.
-- the maximum value will be @bins@ * @width@, and larger values
-- are simply clamped.
histogram :: Int -> Double -> [Double] -> Vector Int
histogram bins width ps = V.create $ do
  hist <- M.replicate bins (0 :: Int)
  let
    addpoint p = do
      let bin = min maxVal (floor (p /  width))
      val <- M.read hist bin
      M.write hist bin (val + 1)

  mapM_ addpoint ps
  return hist
  where
    maxVal = bins - 1

histogram' :: Int -> [Double] -> [Int]
histogram' bins ps = V.toList $ histogram bins (maximum ps / fromIntegral bins) ps

calculateA3 :: SV.Vector Point -> StdGen -> [Double]
calculateA3 ps g = map (\(a,b,c) -> angle a b c) pointTrios
  where
    samples = samplePoints g ps
    trioSize = min maxTrios (SV.length samples)
    trios = randomTrios trioSize (SV.length samples - 1) g
    pointTrios = [(samples SV.! i, samples SV.! j, samples SV.! k) | (i, j, k) <- trios]

calculateD2 :: SV.Vector Point -> StdGen -> [Double]
calculateD2 ps g = map (uncurry distance) pointPairs
  where
    samples = samplePoints g ps
    pairSize = min maxPairs (SV.length samples)
    pairs = randomPairs pairSize (SV.length samples - 1) g
    pointPairs = [(samples SV.! i, samples SV.! j) | (i, j) <- pairs]

-- | Sample unique points from the vector. The max of the length or @maxSamples@
samplePoints :: (SV.Storable a) => StdGen -> SV.Vector a -> SV.Vector a
samplePoints g ps = if SV.length ps <= maxSamples
                       then ps
                       else SV.fromList samples
  where samples = [ ps SV.! i | i <- randomIdx maxSamples (SV.length ps - 1) g]

-- | Get a list of size @n@ of trios of indices in the range @0 <= i <= 2@,
-- where for a pair @(x, y)@, @x /= y@
randomTrios :: Int -> Int -> StdGen -> [(Int, Int, Int)]
randomTrios n s g = take n $ filter uniques $ zip3 (randomRs (0, s) g)  (randomRs (0, s) g') (randomRs (0, s) g'')
  where
    (g', g'') = split g
    uniques (a, b, c) = a /= b && a /= c && b /= c

-- | Get a list of size @n@ of pairs of indices in the range @0 <= i <= 2@,
-- where for a pair @(x, y)@, @x /= y@
randomPairs :: Int -> Int -> StdGen -> [(Int, Int)]
randomPairs n s g = take n $ filter uniques $ zip (randomRs (0, s) g') (randomRs (0, s) g'')
  where
    (g', g'') = split g
    uniques (a, b) = a /= b

-- | Get a list of size @n@ of uniques indices in the range @0 <= i <= 2@
randomIdx :: Int -> Int -> StdGen -> [Int]
randomIdx n s  = take n . randomUniques (0, s)

-- | Get an infinite list of unique random elements in bound @bnds@
randomUniques :: (Random a, Eq a) => (a, a) -> StdGen -> [a]
randomUniques bnds = nub . randomRs bnds
