module Ifrit.Drivers.MongoDB where

import Prelude

import Control.Monad.State(StateT, get, lift, put, runStateT)
import Data.Argonaut.Core(Json, jsonZero, jsonNull)
import Data.Argonaut.Encode(encodeJson, (:=), (~>))
import Data.Array(snoc, foldM)
import Data.Bifunctor(lmap)
import Data.Either(Either(..))
import Data.List(fromFoldable) as L
import Data.Maybe(Maybe(..), maybe)
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


-- INSTANCE INGEST
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
        ingest' name t = do
          t' <- ingest t
          schema <- get
          case schema of
            JNumber ->
              pure $ singleton ("$" <> name) t'
            _ ->
              lift $ Left ("invalid operation @" <> name <> ": target `=` isn't a number")
    in case r of
        Avg t ->
          ingest' "avg" t
        Min t ->
          ingest' "min" t
        Max t ->
          ingest' "max" t


instance ingestMap :: Ingest Map where
  ingest m =
    let
        inject src target fn = do
          jsonSrc <- ingest src
          schemaSrc <- get
          case schemaSrc of
            JArray schemaSrcElem -> do
              put schemaSrcElem
              _ <- ingest target
              schemaTarget <- get
              fn jsonSrc (selector target) schemaTarget
            _ ->
              lift $ Left "invalid operation @inject: list `[]` isn't an array"
    in case m of
        Project term ->
          ingest term

        Add terms ->
          let
              check schema term = do
                jsonTerm <- ingest term
                schemaTerm <- get
                case schemaTerm of
                  JNumber -> do
                    put schema
                    pure jsonTerm
                  _ ->
                    lift $ Left "invalid operation @add: at least one element isn't a number"
          in do
              schema <- get
              terms' <- traverse (check schema) terms
              put JNumber
              pure $ singleton "$add" (encodeJson terms')

        Inject src (Avg target) ->
          let
              fn jsonSrc jsonTarget schemaTarget =
                case schemaTarget of
                  JNumber ->
                    let
                        sum = list
                          [ encodeJson "$$value"
                          , jsonTarget
                          ]
                        reduce = object
                          [ "input" := jsonSrc
                          , "initialValue" := jsonZero
                          , "in" := (singleton "$add" sum)
                          ]
                        divide = list
                          [ singleton "$reduce" reduce
                          , singleton "$size" jsonSrc
                          ]
                    in do
                        pure $ singleton "$divide" divide
                  _ ->
                    lift $ Left "invalid operation @avg: target `=` isn't a number"
          in
              inject src target fn

        Inject src (Min target) ->
          let
              fn jsonSrc jsonTarget schemaTarget =
                case schemaTarget of
                  JNumber ->
                    let
                        or = list
                          [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                          , singleton "$lt" (list [ jsonTarget, encodeJson "$$value" ])
                          ]
                        cond = object
                          [ "if" := (singleton "$or" or)
                          , "then" := jsonTarget
                          , "else" := (encodeJson "$$value")
                          ]
                        reduce = object
                          [ "input" := jsonSrc
                          , "initialValue" := jsonNull
                          , "in" := (singleton "$cond" cond)
                          ]
                    in do
                        pure $ singleton "$reduce" reduce
                  _ ->
                    lift $ Left "invalid operation @min: target `=` isn't a number"
          in
              inject src target fn

        Inject src (Max target) ->
          let
              fn jsonSrc jsonTarget schemaTarget =
                case schemaTarget of
                  JNumber ->
                    let
                        or = list
                          [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                          , singleton "$gt" (list [ jsonTarget, encodeJson "$$value" ])
                          ]
                        cond = object
                          [ "if" := (singleton "$or" or)
                          , "then" := jsonTarget
                          , "else" := (encodeJson "$$value")
                          ]
                        reduce = object
                          [ "input" := jsonSrc
                          , "initialValue" := jsonNull
                          , "in" := (singleton "$cond" cond)
                          ]
                    in do
                        pure $ singleton "$reduce" reduce
                  _ ->
                    lift $ Left "invalid operation @max: target `=` isn't a number"
          in
              inject src target fn


instance ingestStage :: Ingest Stage where
  ingest (Map m) = do
    schema <- get
    let f  = (flip runStateT $ schema) :: Pipeline -> Either String (Tuple Json JsonSchema)
    case traverse f (map ingest m) of
      Left err ->
        lift $ Left err
      Right obj -> do
        put $ JObject (map snd obj)
        pure $ (singleton "$project" $ encodeJson (map fst obj))

  ingest (Reduce index m) = do
    jsonIndex <- maybe (pure jsonNull) ingest index
    schema <- get
    let f  = (flip runStateT $ schema) :: Pipeline -> Either String (Tuple Json JsonSchema)
    case traverse f (map ingest m) of
      Left err ->
        lift $ Left err
      Right obj -> do
        put $ JObject (map snd obj)
        pure $ singleton "$group" (("_id" := jsonIndex) ~> encodeJson (map fst obj))


instance ingestArray :: Ingest a => Ingest (Array a) where
  ingest xs =
    let
        foldStage :: Tuple (Array Json) JsonSchema -> a -> Either String (Tuple (Array Json) JsonSchema)
        foldStage (Tuple queue schema) step =
          lmap (snoc queue) <$> runStateT (ingest step) schema
    in do
        schema <- get
        case foldM foldStage (Tuple [] schema) xs of
          Right (Tuple jsons schema') -> do
            put schema'
            pure $ encodeJson jsons
          Left err ->
            lift $ Left err
