-- |

module Main where

import qualified Data.Vector.Storable as SV
import           System.Environment
import           System.Random

import           Convex
import           Database
import           Features
import           Normals
import           Point
import           Sampling

main :: IO ()
main = do
  --files <- getArgs
  --input <- readFile (head files)
  --g <- newStdGen
  --let points = readPoints $ lines input
  --putStrLn "D2 shape: "
  --mapM_ print $ calculateD2 points g
  --print $ histogram 14 10 (calculateD2 points g)
  print $ convexHull $ SV.fromList [(P 1 0 0), (P 0 0 1), (P 0 0 (-1)), (P 0.5 1 0), (P 0.5 0.5 0)]
readPoints :: [String] -> SV.Vector Point
readPoints = SV.fromList . map (makePoint . map read . words)
