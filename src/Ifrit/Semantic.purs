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


fromString :: String -> Either String Schema
fromString =
  jsonParser >=> fromJson

fromJson :: Json -> Either String Schema
fromJson =
  decodeJson


lookupProjection :: Schema -> StrMap Schema -> Parser.Projection -> Either String (StrMap Schema)
lookupProjection schema acc (Parser.Projection selector) =
  let
      lookupNumberProjection :: Partial => StrMap Schema -> Parser.Selector -> Either String (StrMap Schema)
      lookupNumberProjection source (Parser.Function f key as) =
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
  in
      case { schema, selector } of
        { schema: Object source, selector: Parser.Selector key as } ->
          case StrMap.lookup key source of
            Just schema' ->
              Right $ StrMap.insert (maybe key (\x -> x) as) schema' acc

            Nothing ->
              Left $ "unexisting field: '" <> key <> "'"

        { schema: Object source, selector: f@(Parser.Function Lexer.Avg key as) } ->
          unsafePartial $ lookupNumberProjection source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Count key as) } ->
          unsafePartial $ lookupNumberProjection source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Max key as) } ->
          unsafePartial $ lookupNumberProjection source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Min key as) } ->
          unsafePartial $ lookupNumberProjection source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Sum key as) } ->
          unsafePartial $ lookupNumberProjection source f

        _ ->
          Left "invalid operation: can't SELECT on non object"


lookupAggregation :: Schema -> StrMap Schema -> Parser.Aggregation -> Either String (StrMap Schema)
lookupAggregation schema acc (Parser.Aggregation selector) =
  let
      lookupNumberAggregation :: Partial => StrMap Schema -> Parser.Selector -> Either String (StrMap Schema)
      lookupNumberAggregation source (Parser.Function f key as) =
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
  in
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

        { schema: Object source, selector: f@(Parser.Function Lexer.Avg key as) } ->
          unsafePartial $ lookupNumberAggregation source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Count key as) } ->
          unsafePartial $ lookupNumberAggregation source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Max key as) } ->
          unsafePartial $ lookupNumberAggregation source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Min key as) } ->
          unsafePartial $ lookupNumberAggregation source f

        { schema: Object source, selector: f@(Parser.Function Lexer.Sum key as) } ->
          unsafePartial $ lookupNumberAggregation source f

        _ ->
          Left "invalid operation: can't SELECT on non object"


analyze :: Schema -> Parser.Statement -> Either String Schema
analyze schema (Parser.Select projections statement condition) = do
  schema' <- maybe (Right schema) (analyze schema) statement
  schema'' <- List.foldM (lookupProjection schema') StrMap.empty projections
  pure $ Object schema''

analyze schema (Parser.Group index aggregations statement condition) = do
  schema' <- maybe (Right schema) (analyze schema) statement
  schema'' <- List.foldM (lookupAggregation schema') StrMap.empty aggregations
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


instance decodeSchema :: DecodeJson Schema where
  decodeJson =
    let
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
    in
        foldJson decodeNull decodeBoolean decodeNumber decodeString decodeArray decodeObject


instance encodeJsonSchema :: EncodeJson Schema where
  encodeJson (Object schema) =
    encodeJson $ map encodeJson schema
  encodeJson (Array schema) =
    encodeJson [encodeJson schema]
  encodeJson Number =
    encodeJson "number"
  encodeJson String =
    encodeJson "string"
  encodeJson Boolean =
    encodeJson "boolean"
  encodeJson Null =
    encodeJson "null"


instance showSchema :: EncodeJson Schema => Show Schema where
  show = encodeJson >>> stringify
