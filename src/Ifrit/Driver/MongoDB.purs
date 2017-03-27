module Ifrit.Driver.MongoDB
  ( class Ingest
  , Pipeline
  , ingest
  , compile
  ) where

import Prelude

import Control.Apply(lift2)
import Control.Monad.State(evalStateT)
import Data.Argonaut.Core(Json, JAssoc, jsonEmptyObject, jsonZero, jsonNull, toArray)
import Data.Argonaut.Encode(encodeJson, extend, (:=), (~>))
import Data.Array(concat)
import Data.Decimal(toNumber)
import Data.Either(Either(..))
import Data.Foldable(foldr)
import Data.List(List)
import Data.List as List
import Data.Maybe(Maybe(..), maybe)
import Data.StrMap as StrMap
import Data.String(Pattern(..), split)
import Data.Tuple(Tuple(..))
import Partial.Unsafe(unsafePartial)

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer


-- CLASS & TYPES
type Pipeline = Either String Json

class Ingest stage where
  ingest :: stage -> Pipeline

compile :: String -> Pipeline
compile query = do
  tokens <- evalStateT Lexer.tokenize { pos: 0, str: query }
  ast :: Parser.Statement <- evalStateT Parser.parse tokens
  ingest ast


-- UTILITIES
object :: Array (Tuple String Json) -> Json
object = StrMap.fromFoldable >>> encodeJson


singleton :: String -> Json -> Json
singleton k x = object [Tuple k x]


list :: Array Json -> Json
list = List.fromFoldable >>> encodeJson


defaultAlias :: String -> Maybe String -> String
defaultAlias default =
  maybe default (\x -> x)


extendM :: Either String JAssoc -> Either String Json -> Either String Json
extendM = lift2 extend


ingestBinary :: Lexer.Binary -> String
ingestBinary Lexer.Eq = "$eq"
ingestBinary Lexer.Neq = "$neq"
ingestBinary Lexer.Lt = "$lt"
ingestBinary Lexer.Gt = "$gt"


ingestProjection' :: Parser.Projection -> Either String JAssoc
ingestProjection' (Parser.Projection selector) =
  case selector of
    Parser.Selector s as ->
      Right $ defaultAlias s as := ("$" <> s)

    Parser.Function Lexer.Avg s as ->
      case split (Pattern ".") s of
        [source, target] ->
          let
              alias = defaultAlias target as
              sum = list
                [ encodeJson "$$value"
                , encodeJson $ "$" <> target
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> source)
                , "initialValue" := jsonZero
                , "in" := (singleton "$add" sum)
                ]
              divide = list
                [ singleton "$reduce" reduce
                , singleton "$size" (encodeJson source)
                ]
          in
              Right $ alias := singleton "divide" divide
        _ ->
          Right $ defaultAlias s as := singleton "$avg" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Count s as ->
      case s of
        "*" ->
          Right $ defaultAlias "count" as := singleton "$sum" (encodeJson 1)
        _ ->
          let
              alias = defaultAlias s as
              count = list
                [ encodeJson "$$value"
                , encodeJson 1
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> s)
                , "initialValue" := jsonZero
                , "in" := (singleton "$add" count)
                ]
          in
              Right $ alias := singleton "$reduce" reduce

    Parser.Function Lexer.Max s as ->
      case split (Pattern ".") s of
        [source, target] ->
          let
              alias = defaultAlias target as
              or = list
                [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                , singleton "$gt" (list [ encodeJson $ "$" <> target, encodeJson "$$value" ])
                ]
              cond = object
                [ "if" := (singleton "$or" or)
                , "then" := (encodeJson $ "$" <> target)
                , "else" := (encodeJson "$$value")
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> source)
                , "initialValue" := jsonNull
                , "in" := (singleton "$cond" cond)
                ]
          in
              Right $ alias := singleton "$reduce" reduce
        _ ->
          Right $ defaultAlias s as := singleton "$max" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Min s as ->
      case split (Pattern ".") s of
        [source, target] ->
          let
              alias = defaultAlias target as
              or = list
                [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                , singleton "$lt" (list [ encodeJson $ "$" <> target, encodeJson "$$value" ])
                ]
              cond = object
                [ "if" := (singleton "$or" or)
                , "then" := (encodeJson $ "$" <> target)
                , "else" := (encodeJson "$$value")
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> source)
                , "initialValue" := jsonNull
                , "in" := (singleton "$cond" cond)
                ]
          in
              Right $ alias := singleton "$reduce" reduce
        _ ->
          Right $ defaultAlias s as := singleton "$min" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Sum s as ->
      case split (Pattern ".") s of
        [source, target] ->
          let
              alias = defaultAlias target as
              count = list
                [ encodeJson "$$value"
                , encodeJson $ "$$this." <> target
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> source)
                , "initialValue" := jsonZero
                , "in" := (singleton "$add" count)
                ]
          in
              Right $ alias := singleton "$reduce" reduce
        _ ->
          Right $ defaultAlias s as := singleton "$sum" (encodeJson $ "$" <> s)


ingestAggregation' :: Parser.Aggregation -> Either String JAssoc
ingestAggregation' (Parser.Aggregation selector) =
  case selector of
    Parser.Selector s as ->
      Right $ defaultAlias s as := singleton "$push" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Avg s as ->
      Right $ defaultAlias s as := singleton "$avg" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Count s as ->
      Right $ defaultAlias "count" as := singleton "$sum" (encodeJson 1)

    Parser.Function Lexer.Max s as ->
      Right $ defaultAlias s as := singleton "$max" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Min s as ->
      Right $ defaultAlias s as := singleton "$min" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Sum s as ->
      Right $ defaultAlias s as := singleton "$sum" (encodeJson $ "$" <> s)


unwrapStatement :: Json -> Array Json
unwrapStatement json = unsafePartial $
  case (toArray json) of
    Just xs ->
      xs


maybeIngest :: forall a. Ingest a
  => (Json -> Array Json) -> Maybe a -> Either String (Array Json)
maybeIngest fn =
  maybe (pure []) (ingest >=> (\x -> pure $ fn x))


instance ingestStatement :: Ingest Parser.Statement where
  ingest (Parser.Select projections statement condition) = do
    projections' <- ingest projections
    statement' <- maybeIngest unwrapStatement statement
    condition' <- maybeIngest (\x -> [singleton "$match" x]) condition
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , [ singleton "$project" projections' ]
      ]

  ingest (Parser.Group index aggregations statement condition) = do
    aggregations' <- ingest aggregations
    statement' <- maybeIngest unwrapStatement statement
    condition' <- maybeIngest (\x -> [singleton "$match" x]) condition
    index' <- ingest index
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , [ singleton "$group" (("_id" := index') ~> aggregations') ]
      ]


instance ingestProjection :: Ingest (List Parser.Projection) where
  ingest xs =
    let
        projections = map ingestProjection' xs
        init = Right jsonEmptyObject
    in
        map encodeJson (foldr extendM init projections)


instance ingestAggregation :: Ingest (List Parser.Aggregation) where
  ingest xs =
    let
        aggregations= map ingestAggregation' xs
        init = Right jsonEmptyObject
    in
        map encodeJson (foldr extendM init aggregations)


instance ingestCondition :: Ingest Parser.Condition where
  ingest (Parser.Term t) = do
    t' <- ingest t
    pure t'

  ingest (Parser.Or t1 t2) = do
    t1' <- ingest t1
    t2' <- ingest t2
    pure $ singleton "$or" (list [t1', t2'])


instance ingestTerm :: Ingest Parser.Term where
  ingest (Parser.Factor f) = do
    f' <- ingest f
    pure f'

  ingest (Parser.And f1 f2) = do
    f1' <- ingest f1
    f2' <- ingest f2
    pure $ singleton "$and" (list [f1', f2'])


instance ingestFactor :: Ingest Parser.Factor where
  ingest (Parser.Operand o) = do
    o' <- ingest o
    pure o'

  ingest (Parser.Binary op o1 o2) = do
    o1' <- ingest o1
    o2' <- ingest o2
    let op' = ingestBinary op
    pure $ singleton op' (list [o1', o2'])


instance ingestOperand :: Ingest Parser.Operand where
  ingest (Parser.String s) =
    pure $ encodeJson s
  ingest (Parser.Boolean b) =
    pure $ encodeJson b
  ingest (Parser.Number d) =
    pure $ encodeJson $ toNumber d
  ingest (Parser.Field s) =
    pure $ encodeJson $ "$" <> s
  ingest (Parser.Null) =
    pure jsonNull
  ingest (Parser.Condition c) =
    ingest c


instance ingestIndex :: Ingest Parser.Index where
  ingest (Parser.IdxField s) =
    pure $ encodeJson $ "$" <> s
  ingest (Parser.IdxNull) =
    pure jsonNull
