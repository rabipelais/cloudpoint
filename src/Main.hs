-- |

module Main where

import qualified Data.Vector.Storable as SV
import           System.Environment
import           System.Random

import           Normals
import           Point
import           Sampling

main :: IO ()
main = do
  files <- getArgs
  input <- readFile (head files)
  g <- newStdGen
  let points = readPoints $ lines input
  putStrLn "D2 shape: "
  mapM_ print $ calculateD2 points g
  print $ histogram 14 10 (calculateD2 points g)

readPoints :: [String] -> SV.Vector Point
readPoints = SV.fromList . map (makePoint . map read . words)
