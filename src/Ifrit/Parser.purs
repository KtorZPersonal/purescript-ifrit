module Ifrit.Parser where

import Prelude

import Data.Maybe(Maybe(..), maybe)
import Data.Either(Either(..))
import Data.Array(fromFoldable, intercalate)
import Control.Monad.State(StateT, put, get, lift)
import Data.List(List(..), (:), snoc)
import Data.Tuple(Tuple(..), fst, snd)

import Ifrit.Lexer(Token(..))
import Ifrit.Lexer(Keyword(..)) as K
import Ifrit.Lexer(BooleanOp(..)) as O
import Ifrit.Lexer(Parenthesis(..)) as P
import Ifrit.Core(JsonSchema(..))

data Query
  = Select (List Selector) (Maybe Query) (Maybe Filter) (Maybe Group)

data Filter
  = Where Condition

data Group
  = GroupBy Operand

data Selector
  = Field String
  | FieldAs String String
  | Function Func String String

data Func
  = Avg
  | Count
  | Max
  | Min
  | Sum

data Condition
  = ConditionTerm Operator
  | And Condition Condition
  | Or Condition Condition

data Operator
  = Eq Operand Operand
  | Neq Operand Operand
  | Gt Operand Operand
  | Lt Operand Operand

data Operand
  = OpString String
  | OpBoolean Boolean
  | OpNumber Number
  | OpNull
  | OpSelector Selector

type Clause a = StateT (Tuple JsonSchema (List Token)) (Either String) a

parse :: Clause Query
parse = do
  Tuple schema tokens <- get
  case tokens of
    (Keyword K.Select : q) -> do
      put $ Tuple schema (Comma : q)
      selectors <- parseSelectors
      query <- parseFrom
      filter <- parseWhere
      pure $ Select selectors query filter Nothing

    (token:q) ->
      lift $ Left ("parsing error: invalid token: " <> (show token))

    Nil ->
      lift $ Left "parsing error: empty query"


parseSelectors :: Clause (List Selector)
parseSelectors = do
  Tuple schema tokens <- get
  case tokens of
    (Comma : (Word w) : (Keyword K.As) : (Word w') : q) -> do
      put $ Tuple schema q
      selectors <- parseSelectors
      pure $ (FieldAs w w') : selectors

    (Comma : (Word w) : q) -> do
      put $ Tuple schema q
      selectors <- parseSelectors
      pure $ (Field w) : selectors

    (Comma : q) -> do
      put $ Tuple schema q
      pure Nil
    _ ->
      pure Nil


parseFrom :: Clause (Maybe Query)
parseFrom = do
  Tuple schema tokens <- get
  case tokens of
    (Keyword K.From : Parenthesis P.Open : q) -> do
      put $ Tuple schema q
      query <- parse
      Tuple schema' tokens' <- get
      case tokens' of
        (Parenthesis P.Close : q') -> do
          put $ Tuple schema' q'
          pure $ Just query
        _ ->
          lift $ Left "parsing error: missing closing parenthesis for FROM clause"

    (Keyword K.From : q) -> do
      lift $ Left "parsing error: FROM clause should be wrapped in parenthesis"

    _ ->
      pure Nothing


parseWhere :: Clause (Maybe Filter)
parseWhere = do
  Tuple schema tokens <- get
  case tokens of
    (Keyword K.Where : Parenthesis P.Open : q) -> do
      put $ Tuple schema q
      condition <- parseCondition
      Tuple schema' tokens' <- get
      case tokens' of
        (Parenthesis P.Close : q') -> do
          put $ Tuple schema' q'
          pure $ Just (Where condition)
        _ ->
          lift $ Left "parsing error: missing closing parenthesis for WHERE clause"

    (Keyword K.Where : q) -> do
      put $ Tuple schema q
      condition <- parseCondition
      pure $ Just (Where condition)

    _ ->
      pure Nothing


parseCondition :: Clause Condition
parseCondition = do
  Tuple schema tokens <- get
  lift $ Left "not implemented"
--    (Word w : BooleanOp b : Word w : q) ->
--
--    (Word w : BooleanOp O.Eq : Null : q) ->
--    (Word w : BooleanOp O.Neq : Null : q) ->
--    (Null : BooleanOp O.Eq : Word w : q) ->
--    (Null : BooleanOp O.Neq : Word w : q) ->
--
--    (And
--
--    (Null : BooleanOp b : q)
--    (_ : BooleanOp b : Null : q) ->


-- INSTANCES
instance showQuery :: Show Query where
  show (Select xs query filter _) =
    let
        query' = maybe "" (\q -> "\nFROM (" <> (show q) <> ")") query
        filter' = maybe "" (\f -> "\n" <> (show f)) filter
    in
        "SELECT " <> (intercalate ", " (map show xs))
        <> query'
        <> filter'

instance showSelector :: Show Selector where
  show (Field s) =
    s
  show (FieldAs s as) =
    s <> " AS " <> as
  show (Function f s s') =
    (show f) <> "(" <> s <> "." <> s' <> ")"

instance showFilter :: Show Filter where
  show (Where c) =
    "WHERE " <> (show c)

instance showCondition :: Show Condition where
  show (ConditionTerm c) =
    show c
  show (And c1 c2) =
    show c1 <> " AND " <> show c2
  show (Or c1 c2) =
    show c1 <> " OR " <> show c2

instance showOperator :: Show Operator where
  show (Eq a b) =
    show a <> " == " <> show b
  show (Neq a b) =
    show a <> " /= " <> show b
  show (Lt a b) =
    show a <> " < " <> show b
  show (Gt a b) =
    show a <> " > " <> show b

instance showOperand :: Show Operand where
  show (OpString s) =
    s
  show (OpNumber n) =
    show n
  show OpNull =
    "null"
  show (OpBoolean b) =
    show b
  show (OpSelector s) =
    show s

instance showFunc :: Show Func where
  show Avg =
    "AVG"
  show Count =
    "COUNT"
  show Max =
    "MAX"
  show Min =
    "MIN"
  show Sum =
    "SUM"
