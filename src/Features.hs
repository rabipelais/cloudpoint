-- |

module Features where

import           Data.Vector.Storable (Vector)
import           System.Random

import           Convex
import           Point
import           Sampling

data Object = Object String Features deriving (Show)

newtype Features = Features [Feature] deriving (Show)

data Feature = D2 !Int [Int]
             | A3 !Int [Int]
             | CH !Double !Double -- area, volume
             deriving (Show)


d2Feature :: Int -> StdGen -> Vector Point -> Feature
d2Feature bins g ps = D2 bins $ histogram' bins $ calculateD2 ps g

a3Feature :: Int -> StdGen -> Vector Point -> Feature
a3Feature bins g ps = A3 bins $ histogram' bins $ calculateA3 ps g

convexFeature :: Vector Point -> Feature
convexFeature ps = CH a v
  where
    a = _area ch
    v = _volume ch
    ch = convexHull ps

-- | Compare two histograms, only if they have the same number of bins
-- using the mean square error
diffHistograms :: (Int, [Int]) -> (Int, [Int]) -> Maybe Int
diffHistograms (b, h) (b', h') = if b == b'
                                  then Just $ mse h h'
                                  else Nothing
  where
    mse :: [Int] -> [Int] -> Int
    mse xs ys = sum $ map (^(2 :: Integer)) $ zipWith (-) xs ys
