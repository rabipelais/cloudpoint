-- |

module Database where

import           Text.XML.HXT.Core

import           Features

xmlFeatures :: ArrowXml a => [Feature] -> a n XmlTree
xmlFeatures fs =
  selem "features" $ map xmlFeature fs
  where
    xmlFeature (D2 bins hist) =
      mkelem "d2" [attr "bins" (txt (show bins))] $
        map (\i -> selem "value" [txt $ show i]) hist
    xmlFeature (A3 bins hist) =
      mkelem "a3" [attr "bins" (txt (show bins))] $
        map (\i -> selem "value" [txt $ show i]) hist
    xmlFeature (CH area volume) =
      selem "ch" [selem "area" [txt (show area)]
                 ,selem "volume" [txt (show volume)]]
