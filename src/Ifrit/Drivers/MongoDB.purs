module Ifrit.Drivers.MongoDB
  ( class Ingest
  , ingest
  , ingestType
  ) where

import Prelude
import Data.Argonaut.Core(Json, stringify)
import Data.Argonaut.Encode(encodeJson, (:=))
import Data.Array(foldM)
import Data.StrMap(fromFoldable, lookup) as M
import Data.List(fromFoldable) as L
import Data.Tuple(Tuple(..))
import Ifrit.Core(JsonSchema(..), Stage(..), Reduce(..), Map(..), Terminal(..))
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))
import Data.Traversable(traverse)


-- CLASSES

class Ingest stage where
  ingest :: stage -> Json
  ingestType :: JsonSchema -> stage -> Either String JsonSchema

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

  ingestType schema (Map m) =
    JObject <$> traverse (ingestType schema) m


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

  ingestType schema (Project t) =
    ingestType schema t
  ingestType schema (Inject src (Avg target)) =
    case ingestType schema src of
    Right (JArray schema') ->
      ingestType schema' target
    Right _ ->
      Left ("invalid non-array source []" <> (stringify $ encodeJson src))
    Left err ->
      Left err


instance ingestReduce :: Ingest Reduce where
  ingest (Avg t) =
    singleton "$avg" $ ingest t

  ingestType schema (Avg t) =
    case ingestType schema t of
    Right JNumber ->
      Right JNumber
    Right _ ->
      Left "invalid operation @avg on non-number"
    Left err ->
      Left err


instance ingestTerminal :: Ingest Terminal where
  ingest (Field f) =
    encodeJson $ "$" <> f
  ingest (ConstantString c) =
    encodeJson  c
  ingest (ConstantBoolean c) =
    encodeJson c
  ingest (ConstantNumber c) =
    encodeJson c

  ingestType (JObject schema) (Field f) =
    case M.lookup f schema of
    Just schema' ->
      Right schema'
    Nothing ->
      Left ("unexisting reference to @field=" <> f)
  ingestType schema (Field f) =
    Left "invalid operation @field on non-object"
  ingestType _ (ConstantString c) =
    Right JString
  ingestType _ (ConstantNumber c) =
    Right JNumber
  ingestType _ (ConstantBoolean c) =
    Right JBoolean


instance ingestArray :: Ingest a => Ingest (Array a) where
  ingest =
    L.fromFoldable >>> map ingest >>> encodeJson

  ingestType =
    foldM ingestType
