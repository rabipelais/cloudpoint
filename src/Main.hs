-- | 

module Main where
                                         
import System.Environment
import System.Random      
import qualified Data.Vector as V

import Point
import Sampling

main :: IO ()
main = do 
  files <- getArgs
  input <- readFile (head files)
  g <- newStdGen
  let points = readPoints $ lines input
  putStrLn "D2 shape: "
  mapM_ print $ calculateD2 points g
  print $ histogram 14 10 (calculateD2 points g) 

readPoints :: [String] -> V.Vector Point
readPoints = V.fromList . map (makePoint . map read . words)
