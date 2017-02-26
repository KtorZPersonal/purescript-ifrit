module Ifrit.Drivers.MongoDB
  ( class Ingest
  , ingest
  ) where

import Prelude
import Data.Argonaut.Core(Json)
import Data.Argonaut.Encode(encodeJson, (:=))
import Data.StrMap(fromFoldable) as M
import Data.List(fromFoldable) as L
import Data.Tuple(Tuple(..))
import Ifrit.Core(Stage(..), Reduce(..), Map(..), Terminal(..))


-- CLASSES

class Ingest stage where
  ingest :: stage -> Json


-- UTILITIES

object :: Array (Tuple String Json) -> Json
object = M.fromFoldable >>> encodeJson


singleton :: String -> Json -> Json
singleton k x = object [Tuple k x]


list :: Array Json -> Json
list = L.fromFoldable >>> encodeJson


selector :: Terminal -> Json
selector (Field f) = encodeJson ("$$this." <> f)
selector (ConstantString c) = encodeJson c
selector (ConstantBoolean c) = encodeJson c
selector (ConstantNumber c) = encodeJson c


-- INSTANCES :: ingest

instance ingestStage :: Ingest Stage where
  ingest (Map m) =
    singleton "$project" $ encodeJson (map ingest m)


instance ingestMapOperator :: Ingest Map where
  ingest (Project t) =
    ingest t
  ingest (Inject src (Avg target)) =
    let
      src' = ingest src
      sum = list
        [ encodeJson "$$value"
        , selector target
        ]
      reduce = object
        [ "input" := src'
        , "initialValue" := (encodeJson 0.0)
        , "in" := (singleton "$add" sum)
        ]
      divide = list
        [ singleton "$reduce" reduce
        , singleton "$size" src'
        ]
    in
      singleton "$divide" divide


instance ingestReduce :: Ingest Reduce where
  ingest (Avg t) =
    singleton "$avg" $ ingest t


instance ingestTerminal :: Ingest Terminal where
  ingest (Field f) =
    encodeJson $ "$" <> f
  ingest (ConstantString c) =
    encodeJson  c
  ingest (ConstantBoolean c) =
    encodeJson c
  ingest (ConstantNumber c) =
    encodeJson c


instance ingestArray :: Ingest a => Ingest (Array a) where
  ingest =
    L.fromFoldable >>> map ingest >>> encodeJson
