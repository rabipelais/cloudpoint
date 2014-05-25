-- |

module Database where

import           Text.XML.HXT.Arrow.Pickle
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

instance XmlPickler Features where
  xpickle = xpFeatures

instance XmlPickler Feature where
  xpickle = xpFeature

xpFeatures =
  xpElem "features" $
  xpWrap (Features, \(Features fs) -> fs) $
  xpList xpFeature

xpFeature :: PU Feature
xpFeature = PU {appUnPickle = xpChoice PU {appUnPickle = xpChoice xpD2 xpA3 xpLift} xpCH xpLift}

xpD2 :: PU Feature
xpD2 =
  xpElem "d2" $
  xpWrap (uncurry D2, \(D2 bins hist) -> (bins, hist)) $
  xpPair (xpAttr "bins" xpickle) (xpList $ xpElem "value" xpickle)

xpA3 :: PU Feature
xpA3 =
  xpElem "a3" $
  xpWrap (uncurry A3, \(A3 bins hist) -> (bins, hist)) $
  xpPair (xpAttr "bins" xpickle) (xpList $ xpElem "value" xpickle)

xpCH :: PU Feature
xpCH =
  xpElem "ch" $
  xpWrap (uncurry CH, \(CH area vol) -> (area, vol)) $
  xpPair (xpElem "area" xpickle) (xpElem "volume" xpickle)

instance XmlPickler Double where
  xpickle = xpPrim

readXmlFeatures :: XmlTree -> Maybe Features
readXmlFeatures = unpickleDoc xpFeatures
