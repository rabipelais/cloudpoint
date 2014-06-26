{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import qualified Data.Vector.Storable           as SV
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.Random
import           Text.XML.HXT.Core
import           Text.XML.HXT.DOM.FormatXmlTree

import           Convex
import           Database.Mongo
import           Database.XML
import           Features
import           Normals
import           Point
import           Sampling

main :: IO ()
main = do
  files <- getArgs
  inputs <- mapM readFile files
  putStrLn "Read files"
  feats <- mapM (calculateFeatures . readPoints . lines) inputs
  let objects = zipWith Object (map dropExtensions files) feats
  putStrLn "Calculated features"
  toMongo objects


toMongo objects = (insertObjects objects) `inDatabase` "tesis"

toXML objects = do
  [rc] <- runX $ writeToXml objects "features.xml"
  if rc >= c_err
     then exitWith (ExitFailure (negate 1))
     else exitSuccess

readPoints :: [String] -> SV.Vector Point
readPoints = SV.fromList . map (makePoint . map read . words)

calculateFeatures :: SV.Vector Point -> IO Features
calculateFeatures ps = do
  g <- newStdGen
  let fs = map ($ ps) (features g)
  return $ Features fs
  where
    features g =
      [ d2Feature 10 g
      , d2Feature 15 g
      , d2Feature 20 g
      , a3Feature 10 g
      , a3Feature 15 g
      , a3Feature 20 g
      , convexFeature]

--let points = readPoints $ lines input
--putStrLn "D2 shape: "
--mapM_ print $ calculateD2 points g
--print $ histogram 14 10 (calculateD2 points g)
--print $ convexHull $ SV.fromList [(P 1 0 0), (P 0 0 1), (P 0 0 (-1)), (P 0.5 1 0), (P 0.5 0.5 0)]_
