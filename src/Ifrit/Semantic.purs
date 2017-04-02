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
import Data.List(List(..), (:))
import Data.List as List
import Data.Maybe(Maybe(..), maybe)
import Data.Pair(Pair(..))
import Data.StrMap as StrMap
import Data.StrMap(StrMap)
import Data.String(Pattern(..), Replacement(..), replaceAll, split)
import Data.Traversable(traverse)
import Partial.Unsafe(unsafePartial)

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer


-- TYPES AND CLASSES
data Schema
  = Object (StrMap Schema)
  | Array Schema
  | String
  | Number
  | Boolean
  | Null

derive instance eqSchema :: Eq Schema


-- ERRORS
data Error
  = ErrIncompatibleSchema Schema
  | ErrUnexistingField Lexer.Keyword String
  | ErrIncompatibleUnaryType Lexer.Unary Schema
  | ErrIncompatibleBinaryTypes Lexer.Binary Schema Schema
  | ErrIncompatibleFnType Lexer.Funktion Schema
  | ErrInvalidFieldName String
  | ErrReservedFieldName String

instance showError :: Show Error where
  show err =
    case err of
      ErrUnexistingField stage field ->
        "unexisting field '" <> field <> "' in " <> show stage <> " expression"
      ErrIncompatibleSchema schema ->
        "incompatible object schema for operation: " <> show schema
      ErrIncompatibleUnaryType op type_ ->
        "incompatible type " <> show type_ <> " with unary operator " <> show op
      ErrIncompatibleBinaryTypes op type1 type2 ->
        "incompatible types " <> show type1 <> ", " <> show type2 <>
          " with binary operator " <> show op
      ErrIncompatibleFnType fn type_ ->
        "incompatible type " <> show type_ <> " with function " <> show fn
      ErrInvalidFieldName field ->
        "invalid field's name '" <> field <> "'"
      ErrReservedFieldName field ->
        "reserved field's name '" <> field <> "'"


-- UTILS
fromString :: String -> Either String Schema
fromString =
  jsonParser >=> fromJson


fromJson :: Json -> Either String Schema
fromJson =
  decodeJson


defaultAlias :: String -> Maybe String -> String
defaultAlias default =
    maybe (sanitize default) (\x -> x)
  where
    sanitize :: String -> String
    sanitize = replaceAll (Pattern ".") (Replacement "_")


lookup :: Lexer.Keyword -> Schema -> String -> Either String Schema
lookup stage schema key =
  case schema of
    Object source ->
      case List.fromFoldable $ split (Pattern ".") key of
        Nil ->
          Left $ show $ ErrInvalidFieldName key
        h : q ->
          case { q, schema: StrMap.lookup h source } of
            { q: Nil, schema: Just schema' } ->
              pure schema'
            { q: _, schema: Just schema' } ->
              lookup stage schema' (List.intercalate "." q)
            _ ->
              Left $ show $ ErrUnexistingField stage key
    _ ->
      Left $ show $ ErrIncompatibleSchema schema


-- ANALYZERS
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
      let
          extend idx = Object $ StrMap.insert "_id" idx schema''
      in
          map extend (lookup Lexer.GroupBy schema' key)


analyzeOrder :: Schema -> Parser.Order -> Either String Schema
analyzeOrder schema order =
  case order of
    Parser.OrderAsc key ->
      analyze' key
    Parser.OrderDesc key ->
      analyze' key
  where
    analyze' key =
      map (const Null) (lookup Lexer.OrderBy schema key)


analyzeCondition :: Schema -> Parser.Condition -> Either String Schema
analyzeCondition schema condition =
  case condition of
    Parser.Term t ->
      analyzeTerm schema t

    Parser.Or t1 t2 ->
      traverse (analyzeTerm schema) [t1, t2] >>= const (Right Boolean)


analyzeTerm :: Schema -> Parser.Term -> Either String Schema
analyzeTerm schema term =
  case term of
    Parser.Factor f ->
      analyzeFactor schema f

    Parser.And f1 f2 ->
      traverse (analyzeFactor schema) [f1, f2] >>= const (Right Boolean)


analyzeFactor :: Schema -> Parser.Factor -> Either String Schema
analyzeFactor schema factor =
  case factor of
    Parser.Operand o ->
      analyzeOperand schema o

    Parser.Condition c ->
      analyzeCondition schema c

    Parser.Unary op c -> do
      s <- analyzeFactor schema c
      case s of
        Boolean ->
          Right Boolean
        _ ->
          Left $ show $ ErrIncompatibleUnaryType op s

    Parser.Binary op o1 o2 -> do
      Pair s1 s2 <- traverse (analyzeOperand schema) (Pair o1 o2)
      case { op, s1, s2 } of
        { op: Lexer.Gt, s1: Number, s2: Number } ->
          Right Boolean
        { op: Lexer.Lt, s1: Number, s2: Number } ->
          Right Boolean
        { op: Lexer.Gt, s1: _, s2: _ } ->
          Left $ show $ ErrIncompatibleBinaryTypes op s1 s2
        { op: Lexer.Lt, s1: _, s2: _ } ->
          Left $ show $ ErrIncompatibleBinaryTypes op s1 s2
        { op: _, s1: Number, s2: Number } ->
          Right Boolean
        { op: _, s1: Boolean, s2: Boolean } ->
          Right Boolean
        { op: _, s1: String, s2: String } ->
          Right Boolean
        { op: _, s1: _, s2: Null } ->
          Right Boolean
        { op: _, s1: Null, s2: _ } ->
          Right Boolean
        _ ->
          Left $ show $ ErrIncompatibleBinaryTypes op s1 s2


analyzeOperand :: Schema -> Parser.Operand -> Either String Schema
analyzeOperand schema operand =
  case operand of
    Parser.String _ ->
      Right String

    Parser.Boolean _ ->
      Right Boolean

    Parser.Number _ ->
      Right Number

    Parser.Null ->
      Right Null

    Parser.Field key ->
      lookup Lexer.Where schema key


analyzeProjection :: Schema -> StrMap Schema -> Parser.Projection -> Either String (StrMap Schema)
analyzeProjection schema acc (Parser.Projection selector) =
    case selector of
      Parser.Selector key as ->
        case lookup Lexer.Select schema key of
          Right schema' ->
            Right $ StrMap.insert (defaultAlias key as) schema' acc

          Left err ->
            Left $ err

      Parser.Function Lexer.Count key as ->
        case lookup Lexer.Select schema key of
          Right (Array _) ->
            Right $ StrMap.insert (defaultAlias key as) Number acc

          Right schema' ->
            Left $ show $ ErrIncompatibleFnType Lexer.Count schema'

          Left err ->
            Left $ err

      f@(Parser.Function Lexer.Avg key as) ->
        unsafePartial $ analyzeNumberProjection f

      f@(Parser.Function Lexer.Max key as) ->
        unsafePartial $ analyzeNumberProjection f

      f@(Parser.Function Lexer.Min key as) ->
        unsafePartial $ analyzeNumberProjection f

      f@(Parser.Function Lexer.Sum key as) ->
        unsafePartial $ analyzeNumberProjection f

  where

    analyzeNumberProjection :: Partial => Parser.Selector -> Either String (StrMap Schema)
    analyzeNumberProjection (Parser.Function f key as) =
      case List.fromFoldable $ split (Pattern ".") key of
        _ : Nil ->
          case lookup Lexer.Select schema key of
            Right (Array Number) ->
              Right $ StrMap.insert (defaultAlias key as) Number acc

            Right schema' ->
              Left $ show $ ErrIncompatibleFnType f schema'

            Left err ->
              Left $ err

        base : key' ->
          case lookup Lexer.Select schema base of
            Right (Array schema') ->
              case lookup Lexer.Select schema' (List.intercalate "." key') of
                Right Number ->
                  Right $ StrMap.insert (defaultAlias key as) Number acc

                Right schema'' ->
                  Left $ show $ ErrIncompatibleFnType f schema''

                Left err ->
                  Left $ err

            Right _ ->
              Left $ show $ ErrIncompatibleFnType f schema

            Left err ->
              Left $ err

        _ ->
          Left $ show $ ErrInvalidFieldName key


analyzeAggregation :: Schema -> StrMap Schema -> Parser.Aggregation -> Either String (StrMap Schema)
analyzeAggregation schema acc (Parser.Aggregation selector) =
  case selector of
    Parser.Selector "_id" Nothing ->
      Left $ show $ ErrReservedFieldName "_id"

    Parser.Selector _ (Just "_id") ->
      Left $ show $ ErrReservedFieldName "_id"

    Parser.Function _ "_id" Nothing ->
      Left $ show $ ErrReservedFieldName "_id"

    Parser.Function _ _ (Just "_id") ->
      Left $ show $ ErrReservedFieldName "_id"

    Parser.Selector key as ->
      case lookup Lexer.Select schema key of
        Right schema' ->
          Right $ StrMap.insert (defaultAlias key as) (Array schema') acc

        Left err ->
          Left $ err

    f@(Parser.Function Lexer.Count key as) ->
      case lookup Lexer.Select schema key of
        Right _ ->
          Right $ StrMap.insert (defaultAlias key as) Number acc
        Left err ->
          Left $ err

    f@(Parser.Function Lexer.Avg key as) ->
      unsafePartial $ analyzeNumberAggregation f

    f@(Parser.Function Lexer.Max key as) ->
      unsafePartial $ analyzeNumberAggregation f

    f@(Parser.Function Lexer.Min key as) ->
      unsafePartial $ analyzeNumberAggregation f

    f@(Parser.Function Lexer.Sum key as) ->
      unsafePartial $ analyzeNumberAggregation f

  where

    analyzeNumberAggregation :: Partial => Parser.Selector -> Either String (StrMap Schema)
    analyzeNumberAggregation (Parser.Function f key as) =
      case lookup Lexer.Select schema key of
        Right Number ->
          Right $ StrMap.insert (defaultAlias key as) Number acc

        Right schema' ->
          Left $ show $ ErrIncompatibleFnType f schema'

        Left err ->
          Left $ err


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
        Left ("unknown schema's type" <> s)

      decodeArray xs =
        if length xs /= 1
           then Left "can't decode array: exactly one element is expected"
           else case head xs of
             Nothing ->
              Left "can't decode array: exactly one element is expected"
             Just schema ->
               Array <$> decodeJson schema

      decodeObject obj =
        Object <$> traverse decodeJson obj


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
