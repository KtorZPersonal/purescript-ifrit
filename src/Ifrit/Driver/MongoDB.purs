module Ifrit.Driver.MongoDB where

import Prelude

import Control.Apply(lift2)
import Control.Monad.State(StateT, lift)
import Data.Argonaut.Core(Json, JAssoc, jsonEmptyObject, jsonZero, jsonNull)
import Data.Argonaut.Encode(encodeJson, extend, (:=))
import Data.Either(Either(..))
import Data.Foldable(foldr)
import Data.List as L
import Data.Maybe(Maybe, maybe)
import Data.StrMap as M
import Data.String(Pattern(..), split)
import Data.Tuple(Tuple(..))

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer

class Ingest stage where
  ingest :: stage -> Pipeline

type Pipeline = StateT Json (Either String) Json

-- UTILITIES
object :: Array (Tuple String Json) -> Json
object = M.fromFoldable >>> encodeJson


singleton :: String -> Json -> Json
singleton k x = object [Tuple k x]


list :: Array Json -> Json
list = L.fromFoldable >>> encodeJson


defaultAlias :: String -> Maybe String -> String
defaultAlias default =
  maybe default (\x -> x)


extendM :: Either String JAssoc -> Either String Json -> Either String Json
extendM = lift2 extend


instance ingestSelector :: Ingest Parser.Selector where
  ingest (Parser.Multiple xs) =
    let
        selectors = map ingestSelector' xs
        init = Right jsonEmptyObject
    in
        lift $ map encodeJson (foldr extendM init selectors)

  ingest s =
    lift $ extendM (ingestSelector' s) (Right jsonEmptyObject)


ingestSelector' :: Parser.Selector -> Either String JAssoc
ingestSelector' selector =
  case selector of
    Parser.Single s as ->
      Right $ defaultAlias s as := s

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
              pure $ alias := singleton "$reduce" reduce
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
              pure $ alias := singleton "$reduce" reduce
        _ ->
          Right $ defaultAlias s as := singleton "sum" (encodeJson $ "$" <> s)

    _ ->
      Left $ "invalid selector"
