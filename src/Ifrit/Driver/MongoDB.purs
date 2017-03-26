module Ifrit.Driver.MongoDB
  (compile
  ) where

import Prelude

import Control.Apply(lift2)
import Control.Monad.State(evalStateT, runStateT)
import Data.Argonaut.Core(Json, JAssoc, jsonEmptyObject, jsonZero, jsonNull, toArray)
import Data.Argonaut.Encode(encodeJson, extend, (:=), (~>))
import Data.Array(concat)
import Data.Decimal(toNumber)
import Data.Either(Either(..))
import Data.Foldable(foldr)
import Data.List as L
import Data.Maybe(Maybe(..), maybe)
import Data.StrMap as M
import Data.String(Pattern(..), split)
import Data.Tuple(Tuple(..))
import Partial.Unsafe(unsafePartial)

import Ifrit.Parser as Parser
import Ifrit.Lexer as Lexer


-- CLASS & TYPES
class Ingest stage where
  ingest :: stage -> Pipeline


type Pipeline = Either String Json


compile :: String -> Either String Json
compile query = do
  tokens <- evalStateT Lexer.tokenize { pos: 0, str: query }
  Tuple (ast :: Parser.Statement) tokens' <- runStateT Parser.parse tokens
  if L.length tokens' > 0
    then Left $ "unexpected end of query: " <> (L.intercalate " " (map show tokens'))
    else ingest ast



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


ingestBinary :: Lexer.Binary -> String
ingestBinary Lexer.Eq = "$eq"
ingestBinary Lexer.Neq = "$neq"
ingestBinary Lexer.Lt = "$lt"
ingestBinary Lexer.Gt = "$gt"


ingestSelector' :: Parser.Selector -> Either String JAssoc
ingestSelector' selector =
  case selector of
    Parser.Single s as ->
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
          Right $ defaultAlias s as := singleton "sum" (encodeJson $ "$" <> s)

    _ ->
      Left $ "invalid selector"


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
  ingest (Parser.Select selector statement condition Nothing) = do
    selector' <- ingest selector
    statement' <- maybeIngest unwrapStatement statement
    condition' <- maybeIngest (\x -> [singleton "$match" x]) condition
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , [ singleton "$project" selector' ]
      ]

  ingest (Parser.Select selector statement condition (Just index)) = do
    selector' <- ingest selector
    statement' <- maybeIngest unwrapStatement statement
    condition' <- maybeIngest (\x -> [singleton "$match" x]) condition
    index' <- ingest index
    pure $ encodeJson $ concat
      [ statement'
      , condition'
      , [ singleton "$group" (("_id" := index') ~> selector') ]
      ]


instance ingestSelector :: Ingest Parser.Selector where
  ingest (Parser.Multiple xs) =
    let
        selectors = map ingestSelector' xs
        init = Right jsonEmptyObject
    in
        map encodeJson (foldr extendM init selectors)

  ingest s =
    extendM (ingestSelector' s) (Right jsonEmptyObject)


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
