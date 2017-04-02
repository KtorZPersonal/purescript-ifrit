module Ifrit.Driver.MongoDB
  ( class Ingest
  , ingest
  ) where

import Prelude

import Control.Apply(lift2)
import Data.Argonaut.Core(Json, JAssoc, jsonEmptyObject, jsonZero, jsonNull, jsonTrue, jsonFalse)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Encode(encodeJson, extend, (:=), (~>))
import Data.Array(concat)
import Data.Decimal(toNumber)
import Data.Either(Either(..))
import Data.Foldable(foldr)
import Data.List(List, (:))
import Data.List as List
import Data.Maybe(Maybe(..), maybe)
import Data.StrMap as StrMap
import Data.String(Pattern(..), Replacement(..), split, replaceAll)
import Data.Tuple(Tuple(..))
import Partial.Unsafe(unsafePartial)

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer


-- CLASS & TYPES
class Ingest pipeline stage where
  ingest :: stage -> pipeline

class IngestNot pipeline stage where
  ingestNot :: stage -> pipeline


-- ERRORS
data Error
  = ErrCondition Parser.Factor

instance showError :: Show Error where
  show err =
    case err of
      ErrCondition factor ->
        "invalid condition: " <> show factor <> ": should target a field of the document"


-- UTILITIES
object :: Array (Tuple String Json) -> Json
object = StrMap.fromFoldable >>> encodeJson


singleton :: String -> Json -> Json
singleton k x = object [Tuple k x]


toArray :: forall a. a -> Array a
toArray x =
  [x]


list :: Array Json -> Json
list = List.fromFoldable >>> encodeJson


defaultAlias :: String -> Maybe String -> String
defaultAlias default =
    maybe (sanitize default) (\x -> x)
  where
    sanitize :: String -> String
    sanitize = replaceAll (Pattern ".") (Replacement "_")


extendM :: Either String JAssoc -> Either String Json -> Either String Json
extendM = lift2 extend


ingestBinary :: Lexer.Binary -> String
ingestBinary op =
  case op of
    Lexer.Eq ->
      "$eq"
    Lexer.Neq ->
      "$ne"
    Lexer.Lt ->
      "$lt"
    Lexer.Gt ->
      "$gt"
    Lexer.Lte ->
      "$lte"
    Lexer.Gte ->
      "$gte"


ingestReverseBinary :: Lexer.Binary -> String
ingestReverseBinary op =
  case op of
    Lexer.Eq ->
      "$eq"
    Lexer.Neq ->
      "$ne"
    Lexer.Lt ->
      "$gte"
    Lexer.Gt ->
      "$lte"
    Lexer.Lte ->
      "$gt"
    Lexer.Gte ->
      "$lt"


fromArray :: Json -> Array Json
fromArray json = unsafePartial $
  case (Argonaut.toArray json) of
    Just xs ->
      xs


maybeIngest :: forall a. Ingest (Either String Json) a
  => (Json -> Array Json) -> Maybe a -> Either String (Array Json)
maybeIngest fn =
  maybe (pure []) (ingest >=> (\x -> pure $ fn x))


instance ingestStatement :: Ingest (Either String Json) Parser.Statement where
  ingest (Parser.Select projections statement condition orders limit offset) = do
    projections' :: Array Json <- (singleton "$project" >>> toArray) <$> (ingest projections)
    statement' :: Array Json <- maybeIngest fromArray statement
    condition' :: Array Json <- maybeIngest (\x -> [singleton "$match" x]) condition
    orders' :: Array Json <- if List.length orders == 0
      then Right []
      else (singleton "$sort" >>> toArray) <$> (ingest orders)
    limit' :: Array Json <- maybeIngest (\x -> [singleton "$limit" x]) limit
    offset' :: Array Json <- maybeIngest (\x -> [singleton "$skip" x]) offset
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , orders'
      , limit'
      , offset'
      , projections'
      ]

  ingest (Parser.Group index aggregations statement condition orders limit offset) = do
    aggregations' :: Json <- ingest aggregations
    statement' :: Array Json <- maybeIngest fromArray statement
    condition' :: Array Json <- maybeIngest (\x -> [singleton "$match" x]) condition
    index' :: Json <- ingest index
    orders' :: Array Json <- if List.length orders == 0
      then Right []
      else (singleton "$sort" >>> toArray) <$> (ingest orders)
    limit' :: Array Json <- maybeIngest (\x -> [singleton "$limit" x]) limit
    offset' :: Array Json <- maybeIngest (\x -> [singleton "$skip" x]) offset
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , orders'
      , limit'
      , offset'
      , [ singleton "$group" (("_id" := index') ~> aggregations') ]
      ]


instance ingestList :: Ingest (Either String (Tuple String Json)) a => Ingest (Either String Json) (List a) where
  ingest xs =
      map encodeJson (foldr extendM init xs')
    where
      xs' = map ingest xs
      init = Right jsonEmptyObject


instance ingestProjection :: Ingest (Either String (Tuple String Json)) Parser.Projection where
ingest (Parser.Projection selector) =
  case selector of
    Parser.Selector s as ->
      Right $ defaultAlias s as := ("$" <> s)

    Parser.Function Lexer.Avg s as ->
      case List.fromFoldable $ split (Pattern ".") s of
        source : h : q ->
          let
              target = List.intercalate "." (h : q)
              alias = defaultAlias s as
              sum = list
                [ encodeJson "$$value"
                , encodeJson $ "$$this." <> target
                ]
              reduce = object
                [ "input" := (encodeJson $ "$" <> source)
                , "initialValue" := jsonZero
                , "in" := (singleton "$add" sum)
                ]
              divide = list
                [ singleton "$reduce" reduce
                , singleton "$size" (encodeJson $ "$" <> source)
                ]
          in
              Right $ alias := singleton "$divide" divide
        _ ->
          Right $ defaultAlias s as := singleton "$avg" (encodeJson $ "$" <> s)

    Parser.Function Lexer.Count s as ->
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
      case List.fromFoldable $ split (Pattern ".") s of
        source : h : q ->
          let
              target = List.intercalate "." (h : q)
              alias = defaultAlias s as
              or = list
                [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                , singleton "$gt" (list [ encodeJson $ "$" <> target, encodeJson "$$value" ])
                ]
              cond = object
                [ "if" := (singleton "$or" or)
                , "then" := (encodeJson $ "$$this." <> target)
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
      case List.fromFoldable $ split (Pattern ".") s of
        source : h : q ->
          let
              target = List.intercalate "." (h : q)
              alias = defaultAlias s as
              or = list
                [ singleton "$eq" (list [ encodeJson "$$value", jsonNull ])
                , singleton "$lt" (list [ encodeJson $ "$" <> target, encodeJson "$$value" ])
                ]
              cond = object
                [ "if" := (singleton "$or" or)
                , "then" := (encodeJson $ "$$this." <> target)
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
      case List.fromFoldable $ split (Pattern ".") s of
        source : h : q ->
          let
              target = List.intercalate "." (h : q)
              alias = defaultAlias s as
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


instance ingestAggregation :: Ingest (Either String (Tuple String Json)) Parser.Aggregation where
ingest (Parser.Aggregation selector) =
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


instance ingestOrder :: Ingest (Either String (Tuple String Json)) Parser.Order where
ingest (Parser.OrderAsc f) =
  pure $ f := 1
ingest (Parser.OrderDesc f) =
  pure $ f := -1


instance ingestLimit :: Ingest (Either String Json) Parser.Limit where
ingest (Parser.Limit i) =
  pure $ encodeJson i


instance ingestOffset :: Ingest (Either String Json) Parser.Offset where
ingest (Parser.Offset i) =
  pure $ encodeJson i


instance ingestCondition :: Ingest (Either String Json) Parser.Condition where
  ingest (Parser.Term t) = do
    t' <- ingest t
    pure t'

  ingest (Parser.Or t1 t2) = do
    t1' <- ingest t1
    t2' <- ingest t2
    pure $ singleton "$or" (list [t1', t2'])


instance ingestNotCondition :: IngestNot (Either String Json) Parser.Condition where
  ingestNot (Parser.Term t) = do
    t' <- ingestNot t
    pure t'

  ingestNot (Parser.Or t1 t2) = do
    t1' <- ingestNot t1
    t2' <- ingestNot t2
    pure $ singleton "$and" (list [t1', t2'])


instance ingestTerm :: Ingest (Either String Json) Parser.Term where
  ingest (Parser.Factor f) = do
    f' <- ingest f
    pure f'

  ingest (Parser.And f1 f2) = do
    f1' <- ingest f1
    f2' <- ingest f2
    pure $ singleton "$and" (list [f1', f2'])


instance ingestNotTerm :: IngestNot (Either String Json) Parser.Term where
  ingestNot (Parser.Factor f) = do
    f' <- ingestNot f
    pure f'

  ingestNot (Parser.And f1 f2) = do
    f1' <- ingestNot f1
    f2' <- ingestNot f2
    pure $ singleton "$or" (list [f1', f2'])


instance ingestFactor :: Ingest (Either String Json) Parser.Factor where
  ingest (Parser.Operand o) = do
    case o of
      Parser.Field f ->
        pure $ singleton f jsonTrue
      _ ->
        ingest o

  ingest (Parser.Condition c) = do
    ingest c

  ingest (Parser.Unary op o) = do
    case op of
      Lexer.Not -> do
        ingestNot o

  ingest factor@(Parser.Binary op left right) = do
    case { left, right } of
      { left: Parser.Field f, right: _ } -> do
        right' <- ingest right
        pure $ singleton f $ singleton (ingestBinary op) right'

      { left: _, right: Parser.Field f } -> do
        left' <- ingest left
        pure $ singleton f $ singleton (ingestReverseBinary op) left'

      _ ->
        Left $ show $ ErrCondition factor


instance ingestNotFactor :: IngestNot (Either String Json) Parser.Factor where
  ingestNot (Parser.Operand o) = do
    case o of
      Parser.Field f ->
        pure $ singleton f jsonFalse
      _ ->
        ingest o

  ingestNot (Parser.Condition c) =
    ingestNot c

  ingestNot (Parser.Unary op o) = do
    case op of
      Lexer.Not -> do
        ingestNot o

  ingestNot factor@(Parser.Binary op left right) = do
      case { left, right } of
        { left: Parser.Field f, right: _ } -> do
          right' <- ingest right
          pure $ singleton f $ singleton (ingestBinary (not op)) right'

        { left: _, right: Parser.Field f } -> do
          left' <- ingest left
          pure $ singleton f $ singleton (ingestReverseBinary (not op)) left'

        _ ->
          Left $ show $ ErrCondition factor

    where
      not :: Lexer.Binary -> Lexer.Binary
      not op' =
        case op' of
          Lexer.Eq ->
            Lexer.Neq
          Lexer.Neq ->
            Lexer.Eq
          Lexer.Lt ->
            Lexer.Gte
          Lexer.Gt ->
            Lexer.Lte
          Lexer.Lte ->
            Lexer.Gt
          Lexer.Gte ->
            Lexer.Lt


instance ingestOperand :: Ingest (Either String Json) Parser.Operand where
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


instance ingestIndex :: Ingest (Either String Json) Parser.Index where
  ingest (Parser.IdxField s) =
    pure $ encodeJson $ "$" <> s
  ingest (Parser.IdxNull) =
    pure jsonNull
