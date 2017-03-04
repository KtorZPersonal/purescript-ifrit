module Ifrit.Drivers.MongoDB where

import Prelude

import Control.Monad.State(StateT, get, lift, put, runStateT)
import Data.Argonaut.Core(Json)
import Data.Argonaut.Encode(encodeJson, (:=))
import Data.Array(cons, foldM)
import Data.Bifunctor(lmap)
import Data.Either(Either(..))
import Data.List(fromFoldable) as L
import Data.Maybe(Maybe(..))
import Data.StrMap(StrMap)
import Data.StrMap(lookup, fromFoldable) as M
import Data.Traversable(traverse)
import Data.Tuple(Tuple(..), fst, snd)

import Ifrit.Core(JsonSchema(..), Stage(..), Reduce(..), Map(..), Terminal(..))


-- CLASSES & TYPES

class Ingest operator where
  ingest :: operator -> Pipeline

type Pipeline = StateT JsonSchema (Either String) Json


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


-- INSTANCE Ingest

instance ingestTerminal :: Ingest Terminal where
  ingest t =
    let
      ingest' (JObject obj) (Field f) =
        case M.lookup f obj of
          Just schema' -> do
            put schema'
            pure $ (encodeJson $ "$" <> f)
          Nothing ->
            lift $ Left ("invalid operation @field: unreachable field `" <> f <> "`")

      ingest' _ (Field f) =
        lift $ Left "invalid operation @field: source isn't an object"

      ingest' _ (ConstantString c) = do
        put JString
        pure $ encodeJson c

      ingest' _ (ConstantNumber c) = do
        put JNumber
        pure $ encodeJson c

      ingest' _ (ConstantBoolean c) = do
        put JBoolean
        pure $ encodeJson c
    in do
      schema <- get
      ingest' schema t


instance ingestReduce :: Ingest Reduce where
  ingest r =
    let
      ingest' JNumber (Avg t) = do
        t' <- ingest t
        put JNumber
        pure $ singleton "$avg" t'
      ingest' _ (Avg t) =
        lift $ Left "invalid operation @avg: target `=` isn't a number"
    in do
      schema <- get
      ingest' schema r


instance ingestMap :: Ingest Map where
  ingest m =
    let
      ingest' _ (Project t) =
        ingest t
      ingest' schema (Inject src (Avg target)) =
        case runStateT (ingest src) schema of
        Right (Tuple src' (JArray schemaSrc)) ->
          case runStateT (ingest target) schemaSrc of
            Right (Tuple _ JNumber) ->
              let
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
              in do
                put JNumber
                pure $ singleton "$divide" divide
            Right _ ->
              lift $ Left "invalid operation @avg: target `=` isn't a number"
            Left err ->
              lift $ Left err
        Right _ ->
          lift $ Left "invalid operation @inject: list `[]` isn't an array"
        Left err ->
          lift $ Left err
    in do
      schema <- get
      ingest' schema m


instance ingestStage :: Ingest Stage where
  ingest (Map m) = do
    schema <- get
    let f  = (flip runStateT $ schema) :: Pipeline -> Either String (Tuple Json JsonSchema)
    let m' = (map ingest m) :: StrMap Pipeline
    case traverse f m' of
      Left err ->
        lift $ Left err
      Right obj -> do
        put $ JObject (map snd obj)
        pure $ (singleton "$project" $ encodeJson (map fst obj))


instance ingestArray :: Ingest a => Ingest (Array a) where
  ingest xs =
    let
      foldStage :: Tuple (Array Json) JsonSchema -> a -> Either String (Tuple (Array Json) JsonSchema)
      foldStage (Tuple queue schema) step =
        lmap (flip cons queue) <$> runStateT (ingest step) schema
    in do
      schema <- get
      case foldM foldStage (Tuple [] schema) xs of
        Right (Tuple jsons schema') -> do
          put schema'
          pure $ encodeJson jsons
        Left err ->
          lift $ Left err
