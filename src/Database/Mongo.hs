{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Database.Mongo where

import           Database.MongoDB
import           Features

collection = "objects"


inDatabase f db = do
  pipe <- connect (host "127.0.01")
  e <- access pipe master db f
  close pipe
  print e

insertObjects obs = insertMany collection (map mongoObject obs)

mongoObject :: Object -> Document
mongoObject (Object name (Features fs)) =
  ["name" =: name, "features" =: mongoFeatures fs]

mongoFeatures :: [Feature] -> [Document]
mongoFeatures = map mongoFeature
  where
    mongoFeature (D2 bins hist) =
      ["type" =: "d2", "params" =: ["bins" =: bins], "vals" =: hist]
    mongoFeature (A3 bins hist) =
      ["type" =: "a3", "params" =: ["bins" =: bins], "vals" =: hist]
    mongoFeature (CH area volume) =
      ["type" =: "ch", "area" =: area, "volume" =: volume]

--objectsWithFeature :: (Val v, transformers-0.3.0.0:Control.Monad.IO.Class.MonadIO m) => v -> Action m [Document]
objectsWithFeature f = aggregate collection pipeline
  where
    pipeline = [["$unwind" =: "$features"], ["$match" =: ["features.type" =: f]], ["$group" =: ["_id" =: "$name", "features" =: ["$push" =: "$features"]]]]
