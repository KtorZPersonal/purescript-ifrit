module Ifrit.Semantic
  ( Schema(..)
  , analyze
  , fromJson
  , fromString
  ) where

import Prelude

import Data.Argonaut.Core(Json, foldJson, stringify)
import Data.Argonaut.Decode(class DecodeJson, decodeJson)
import Data.Argonaut.Encode(class EncodeJson, encodeJson)
import Data.Argonaut.Parser(jsonParser)
import Data.Array(head, length)
import Data.Either(Either(..))
import Data.List as List
import Data.Maybe(Maybe(..), maybe)
import Data.Pair(Pair(..))
import Data.StrMap as StrMap
import Data.StrMap(StrMap)
import Data.String(Pattern(..), split)
import Data.Traversable(traverse)
import Partial.Unsafe(unsafePartial)

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer


data Schema
  = Object (StrMap Schema)
  | Array Schema
  | String
  | Number
  | Boolean
  | Null

derive instance eqSchema :: Eq Schema


fromString :: String -> Either String Schema
fromString =
  jsonParser >=> fromJson

fromJson :: Json -> Either String Schema
fromJson =
  decodeJson

analyze :: Schema -> Parser.Statement -> Either String Schema
analyze schema (Parser.Select projections statement condition orders limit offset) = do
  schema' <- maybe (Right schema) (analyze schema) statement
  _ <- maybe (Right Null) (analyzeCondition schema') condition
  _ <- traverse (analyzeOrder schema') orders
  schema'' <- List.foldM (analyzeProjection schema') StrMap.empty projections
  pure $ Object schema''

analyze schema (Parser.Group index aggregations statement condition orders limit offset) = do
  schema' <- maybe (Right schema) (analyze schema) statement
  _ <- maybe (Right Null) (analyzeCondition schema') condition
  _ <- traverse (analyzeOrder schema') orders
  schema'' <- List.foldM (analyzeAggregation schema') StrMap.empty aggregations
  case index of
    Parser.IdxNull ->
      pure $ Object $ StrMap.insert "_id" Null schema''
    Parser.IdxField key ->
      case schema' of
        Object source ->
          case StrMap.lookup key source of
            Just schemaIndex ->
              pure $ Object $ StrMap.insert "_id" schemaIndex schema''
            Nothing ->
              Left $ "unexisting field: '" <> key <> "'"
        _ ->
          Left "invalid operation: can't SELECT on non object"


analyzeOrder :: Schema -> Parser.Order -> Either String Schema
analyzeOrder schema order =
  case order of
    Parser.OrderAsc key ->
      analyze' key
    Parser.OrderDesc key ->
      analyze' key
  where
    analyze' key =
      case schema of
        Object source ->
          case StrMap.lookup key source of
            Just _ ->
              Right Null
            Nothing ->
              Left $ "unexisting field: '" <> key <> "'"
        _ ->
          Left "invalid operation: can't ORDER BY on non object"



analyzeCondition :: Schema -> Parser.Condition -> Either String Schema
analyzeCondition schema condition =
  case condition of
    Parser.Term t ->
      analyzeTerm schema t
    Parser.Or t1 t2 ->
      traverse (analyzeTerm schema) [t1, t2] >>= const (Right Null)


analyzeTerm :: Schema -> Parser.Term -> Either String Schema
analyzeTerm schema term =
  case term of
    Parser.Factor f ->
      analyzeFactor schema f
    Parser.And f1 f2 ->
      traverse (analyzeFactor schema) [f1, f2] >>= const (Right Null)


analyzeFactor :: Schema -> Parser.Factor -> Either String Schema
analyzeFactor schema factor =
  case factor of
    Parser.Operand o ->
      analyzeOperand schema o
    Parser.Binary op o1 o2 -> do
      Pair s1 s2 <- traverse (analyzeOperand schema) (Pair o1 o2)
      case { op, s1, s2 } of
        { op: Lexer.Gt, s1: Number, s2: Number } ->
          Right Null
        { op: Lexer.Lt, s1: Number, s2: Number } ->
          Right Null
        { op: Lexer.Gt, s1: _, s2: _ } ->
          Left $ "invalid combination of types for '" <> show op <> "' operator"
        { op: Lexer.Lt, s1: _, s2: _ } ->
          Left $ "invalid combination of types for '" <> show op <> "' operator"
        { op: _, s1: Number, s2: Number } ->
          Right Null
        { op: _, s1: Boolean, s2: Boolean } ->
          Right Null
        { op: _, s1: String, s2: String } ->
          Right Null
        { op: _, s1: _, s2: Null } ->
          Right Null
        { op: _, s1: Null, s2: _ } ->
          Right Null
        _ ->
          Left $ "invalid combination of types for '" <> show op <> "' operator"


analyzeOperand :: Schema -> Parser.Operand -> Either String Schema
analyzeOperand schema operand =
  case { schema, operand } of
    { schema: _, operand: Parser.String _ } ->
      Right String
    { schema: _, operand: Parser.Boolean _ } ->
      Right Boolean
    { schema: _, operand: Parser.Number _ } ->
      Right Number
    { schema: _, operand: Parser.Null } ->
      Right Null
    { schema: _, operand: Parser.Condition c } ->
      analyzeCondition schema c
    { schema: Object source, operand: Parser.Field key } ->
      case StrMap.lookup key source of
        Just schema' ->
          Right schema'
        Nothing ->
          Left $ "unexisting field: '" <> key <> "'"
    { schema: _, operand: Parser.Field _ } ->
      Left "invalid operation: can't analyze field on non object"


analyzeProjection :: Schema -> StrMap Schema -> Parser.Projection -> Either String (StrMap Schema)
analyzeProjection schema acc (Parser.Projection selector) =
    case { schema, selector } of
      { schema: Object source, selector: Parser.Selector key as } ->
        case StrMap.lookup key source of
          Just schema' ->
            Right $ StrMap.insert (maybe key (\x -> x) as) schema' acc

          Nothing ->
            Left $ "unexisting field: '" <> key <> "'"

      { schema: Object source, selector: f@(Parser.Function Lexer.Count key as) } ->
        case StrMap.lookup key source of
          Just (Array _) ->
            Right $ StrMap.insert (maybe key (\x -> x) as) Number acc
          Just _ ->
            Left $ "invalid operation: function '" <> show f <> "' applied to non array"
          Nothing ->
            Left $ "unexisting field: '" <> key <> "'"

      { schema: Object source, selector: f@(Parser.Function Lexer.Avg key as) } ->
        unsafePartial $ analyzeNumberProjection source f

      { schema: Object source, selector: f@(Parser.Function Lexer.Max key as) } ->
        unsafePartial $ analyzeNumberProjection source f

      { schema: Object source, selector: f@(Parser.Function Lexer.Min key as) } ->
        unsafePartial $ analyzeNumberProjection source f

      { schema: Object source, selector: f@(Parser.Function Lexer.Sum key as) } ->
        unsafePartial $ analyzeNumberProjection source f

      _ ->
        Left "invalid operation: can't SELECT on non object"
  where

    analyzeNumberProjection :: Partial => StrMap Schema -> Parser.Selector -> Either String (StrMap Schema)
    analyzeNumberProjection source (Parser.Function f key as) =
      case split (Pattern ".") key of
        [_] ->
          case StrMap.lookup key source of
            Just (Array Number) ->
              Right $ StrMap.insert (maybe key (\x -> x) as) Number acc
            Just _ ->
              Left $ "invalid operation: function '" <> show f <> "' applied to non array<number>"
            Nothing ->
              Left $ "unexisting field: '" <> key <> "'"

        [base, key'] ->
          case StrMap.lookup base source of
            Just (Array (Object source')) ->
              case StrMap.lookup key' source' of
                Just Number ->
                  Right $ StrMap.insert (maybe key' (\x -> x) as) Number acc
                Just _ ->
                  Left $ "invalid operation: function '" <> show f <> "' applied to non number"
                Nothing ->
                  Left $ "unexisting field: '" <> key <> "'"
            Just _ ->
              Left $ "invalid operation: function '" <> show f <> "': target isn't array<object>"

        _ ->
          Left $ "invalid field's name: '" <> key <> "'"


analyzeAggregation :: Schema -> StrMap Schema -> Parser.Aggregation -> Either String (StrMap Schema)
analyzeAggregation schema acc (Parser.Aggregation selector) =
  case { schema, selector } of
    { selector: Parser.Selector "_id" Nothing } ->
      Left $ "reserved field's name: _id"

    { selector: Parser.Selector _ (Just "_id") } ->
      Left $ "reserved field's name: _id"

    { selector: Parser.Function _ "_id" Nothing } ->
      Left $ "reserved field's name: _id"

    { selector: Parser.Function _ _ (Just "_id") } ->
      Left $ "reserved field's name: _id"

    { schema: Object source, selector: Parser.Selector key as } ->
      case StrMap.lookup key source of
        Just schema' ->
          Right $ StrMap.insert (maybe key (\x -> x) as) (Array schema') acc

        Nothing ->
          Left $ "unexisting field: '" <> key <> "'"

    { schema: Object source, selector: f@(Parser.Function Lexer.Count key as) } ->
      case StrMap.lookup key source of
        Just _ ->
          Right $ StrMap.insert (maybe key (\x -> x) as) Number acc
        Nothing ->
          Left $ "unexisting field: '" <> key <> "'"

    { schema: Object source, selector: f@(Parser.Function Lexer.Avg key as) } ->
      unsafePartial $ analyzeNumberAggregation source f

    { schema: Object source, selector: f@(Parser.Function Lexer.Max key as) } ->
      unsafePartial $ analyzeNumberAggregation source f

    { schema: Object source, selector: f@(Parser.Function Lexer.Min key as) } ->
      unsafePartial $ analyzeNumberAggregation source f

    { schema: Object source, selector: f@(Parser.Function Lexer.Sum key as) } ->
      unsafePartial $ analyzeNumberAggregation source f
    _ ->
      Left "invalid operation: can't SELECT on non object"

  where

    analyzeNumberAggregation :: Partial => StrMap Schema -> Parser.Selector -> Either String (StrMap Schema)
    analyzeNumberAggregation source (Parser.Function f key as) =
      case split (Pattern ".") key of
        [_] ->
          case StrMap.lookup key source of
            Just Number ->
              Right $ StrMap.insert (maybe key (\x -> x) as) Number acc
            Just _ ->
              Left $ "invalid operation: function '" <> show f <> "' applied to non number"
            Nothing ->
              Left $ "unexisting field: '" <> key <> "'"

        _ ->
          Left $ "invalid field's name: '" <> key <> "'"


instance decodeSchema :: DecodeJson Schema where
  decodeJson =
      foldJson decodeNull decodeBoolean decodeNumber decodeString decodeArray decodeObject
    where
      decodeNull _ =
        Left "can't decode null to schema"
      decodeBoolean _ =
        Left "can't decode boolean to schema"
      decodeNumber _ =
        Left "can't decode number to schema"
      decodeString "string" =
        Right String
      decodeString "number" =
        Right Number
      decodeString "boolean" =
        Right Boolean
      decodeString "null" =
        Right Null
      decodeString s =
        Left ("can't decode type: invalid provided type: " <> s)
      decodeArray xs =
        if length xs /= 1
           then Left "can't decode array: exactly one element is expected"
           else case head xs of
             Nothing ->
              Left "can't decode array: exactly one element is expected"
             Just schema ->
               Array <$> decodeJson schema
      decodeObject obj = Object <$> traverse decodeJson obj


instance encodeJsonSchema :: EncodeJson Schema where
  encodeJson schema =
    case schema of
      Object schema' ->
        encodeJson $ map encodeJson schema'
      Array schema' ->
        encodeJson $ [encodeJson schema']
      Number ->
        encodeJson "number"
      String ->
        encodeJson "string"
      Boolean ->
        encodeJson "boolean"
      Null ->
        encodeJson "null"


instance showSchema :: EncodeJson Schema => Show Schema where
  show = encodeJson >>> stringify
