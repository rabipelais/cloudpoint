{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Database.Mongo where

import           Database.MongoDB
import           Features

collection = "objects"


inDatabase :: Show a => Action IO a -> Database -> IO ()
inDatabase f db = do
  pipe <- connect (host "127.0.01")
  e <- access pipe master db f
  close pipe
  print e

fromDatabase :: Action IO b -> Database -> IO b
fromDatabase f db = do
  pipe <- connect (host "127.0.01")
  e <- access pipe master db f
  close pipe
  return e

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

--documentsWithFeature :: (Val v, MonadIO m) => v -> Action m [Document]
documentsWithFeature f = aggregate collection pipeline
  where
    pipeline = [["$unwind" =: "$features"], ["$match" =: ["features.type" =: f]], ["$group" =: ["_id" =: "$name", "features" =: ["$push" =: "$features"]]]]


extractObject :: Document -> Object
extractObject doc = Object name (Features fs)
  where
    name = at "_id" doc
    fs = map extractFeature $ at "features" doc
    extractFeature fdoc
                     | ty == "d2" = D2 (at "bins" (at "params" fdoc)) (at "vals" fdoc)
                     | ty == "a3" = A3 (at "bins" (at "params" fdoc)) (at "vals" fdoc)
                     | ty == "ch" = CH (at "area" fdoc) (at "volume" fdoc)
                     | otherwise = undefined
      where
        ty = at "type" fdoc
