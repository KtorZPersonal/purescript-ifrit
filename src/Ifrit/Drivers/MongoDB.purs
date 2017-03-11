module Ifrit.Drivers.MongoDB where

import Prelude

import Control.Monad.State(StateT, get, lift, put, runStateT)
import Data.Argonaut.Core(Json, jsonNull, jsonZero, toArray)
import Data.Argonaut.Encode(encodeJson, (:=), (~>))
import Data.Array(concat, foldM, snoc)
import Data.Either(Either(..))
import Data.List(fromFoldable) as L
import Data.Maybe(Maybe(..), maybe)
import Data.StrMap(fromFoldable, lookup) as M
import Data.Traversable(traverse)
import Data.Tuple(Tuple(..), fst, snd)

import Ifrit.Core(JsonSchema(..), Stage(..), Reduce(..), Map(..), Filter(..), Terminal(..))


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
              pure $ encodeJson ("$" <> f)
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


instance ingestFilter :: Ingest Filter where
  ingest t =
    let
        -- NOTE Not so happy with this...
        ingestEqWithSchema schema x = do
          x' <- ingest x
          put schema
          pure x'

        ingestCondWithSchema schema x = do
          x' <- ingest x
          put schema
          pure x'
    in do
        schema <- get
        case t of
          Eq terms -> do
            terms' <- traverse (ingestEqWithSchema schema) terms
            put JBoolean
            pure $ singleton "$eq" (encodeJson terms')
          Neq terms -> do
            terms' <- traverse (ingestEqWithSchema schema) terms
            put JBoolean
            pure $ singleton "$neq" (encodeJson terms')
          Gt terms -> do
            terms' <- traverse (ingestEqWithSchema schema) terms
            put JBoolean
            pure $ singleton "$gt" (encodeJson terms')
          Lt terms -> do
            terms' <- traverse (ingestEqWithSchema schema) terms
            put JBoolean
            pure $ singleton "$lt" (encodeJson terms')
          Or filters -> do
            terms' <- traverse (ingestCondWithSchema schema) filters
            put JBoolean
            pure $ singleton "$or" (encodeJson terms')
          And filters -> do
            terms' <- traverse (ingestCondWithSchema schema) filters
            put JBoolean
            pure $ singleton "$and" (encodeJson terms')

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
        Sum t ->
          ingest' "sum" t


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

        nbOp op op' term = do
          jsonTerm <- ingest term
          schema <- get
          case schema of
            JNumber ->
              pure $ singleton ("$" <> op') jsonTerm
            _ ->
              lift $ Left ("invalid operation @" <> op <> ": target `=` isn't a number")

        iterNbOp op op' terms =
          let
              check schema term = do
                jsonTerm <- ingest term
                schemaTerm <- get
                case schemaTerm of
                  JNumber -> do
                    put schema
                    pure jsonTerm
                  _ ->
                    lift $ Left ("invalid operation @" <> op <> ": at least one element isn't a number")
          in do
              schema <- get
              terms' <- traverse (check schema) terms
              put JNumber
              pure $ singleton ("$" <> op') (encodeJson terms')

    in case m of
        Project term ->
          ingest term

        Abs term ->
          nbOp "abs" "abs" term

        Add terms ->
          iterNbOp "add" "add" terms

        Div terms ->
          iterNbOp "div" "divide" terms

        Ceil term ->
          nbOp "ceil" "ceil" term

        Floor term ->
          nbOp "floor" "floor" term

        Mult terms ->
          iterNbOp "mult" "multiply" terms

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
                     in
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
                     in
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
                    in
                        pure $ singleton "$reduce" reduce
                  _ ->
                    lift $ Left "invalid operation @max: target `=` isn't a number"
          in
              inject src target fn

        Inject src (Sum target) ->
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
                  in
                      pure $ singleton "$reduce" reduce
                _ ->
                  lift $ Left "invalid operation @sum: target `=` isn't a number"
          in
              inject src target fn

-- NOTE Each stage is expected to return a encoded json array
instance ingestStage :: Ingest Stage where
  ingest (Map filter m) =
    let
        ingestMatch Nothing =
          pure []
        ingestMatch (Just f) = do
          json <- ingest f
          pure $ [encodeJson (singleton "$match" json)]
    in do
        schema <- get
        jsonFilter <- ingestMatch filter
        let f  = (flip runStateT $ schema) :: Pipeline -> Either String (Tuple Json JsonSchema)
        case traverse f (map ingest m) of
          Left err ->
            lift $ Left err
          Right obj -> do
            put $ JObject (map snd obj)
            pure $ encodeJson (snoc jsonFilter (singleton "$project" $ encodeJson (map fst obj)))

  ingest (Reduce index m) = do
    schema <- get
    jsonIndex <- maybe (pure jsonNull) ingest index
    let f  = (flip runStateT $ schema) :: Pipeline -> Either String (Tuple Json JsonSchema)
    case traverse f (map ingest m) of
      Left err ->
        lift $ Left err
      Right obj -> do
        put $ JObject (map snd obj)
        pure $ encodeJson [singleton "$group" (("_id" := jsonIndex) ~> encodeJson (map fst obj))]


instance ingestArray :: Ingest a => Ingest (Array a) where
  ingest xs =
    let
        concatStages :: forall x. Array Json -> (Tuple Json x)-> Either String (Tuple (Array Json) x)
        concatStages stages (Tuple s x) =
          case toArray s of
            Nothing ->
              Left "invalid ingested stage"
            Just stage ->
              Right $ Tuple (concat [stages, stage]) x

        foldStage :: Tuple (Array Json) JsonSchema -> a -> Either String (Tuple (Array Json) JsonSchema)
        foldStage (Tuple queue schema) step =
          runStateT (ingest step) schema >>= concatStages queue
    in do
        schema <- get
        case foldM foldStage (Tuple [] schema) xs of
          Right (Tuple jsons schema') -> do
            put schema'
            pure $ encodeJson jsons
          Left err ->
            lift $ Left err
