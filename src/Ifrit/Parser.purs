module Ifrit.Parser where

import Prelude

import Control.Alt((<|>))
import Control.Monad.State(StateT, State, put, get, lift)
import Data.Array(fromFoldable, intercalate)
import Data.Either(Either(..))
import Data.List(List(..), (:), snoc)
import Data.Maybe(Maybe(..), maybe)
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

unwrap :: List Token -> Either String (List Token)
unwrap tokens =
  let
      unwrap' 0 k (Parenthesis P.Open : q) Nil =
        unwrap' 0 (k + 1) q Nil
      unwrap' n k (Parenthesis P.Open : q) xs =
        unwrap' (n + 1) k q (snoc xs (Parenthesis P.Open))

      unwrap' 0 1 (Parenthesis P.Close : q) xs =
        Right (xs <|> q)
      unwrap' 0 k (Parenthesis P.Close : q) xs | k > 0 =
        unwrap' 0 (k - 1) q xs
      unwrap' n k (Parenthesis P.Close : q) xs | n > 0 =
        unwrap' (n - 1) k q (snoc xs (Parenthesis P.Close))

      unwrap' n k (h : q) xs =
        unwrap' n k q (snoc xs h)

      unwrap' 0 0 Nil xs =
        Right xs
      unwrap' _ _ Nil Nil =
        Right Nil
      unwrap' _ _ Nil xs =
        Left "parsing error: unbalanced parenthesis expression"
   in
      unwrap' 0 0 tokens Nil


parse :: Clause Query
parse = do
  Tuple schema tokens <- get
  case unwrap tokens of
    Right (Keyword K.Select : q) -> do
      case unwrap q of
        Left err ->
          lift $ Left err
        Right q' -> do
          put $ Tuple schema (Comma : q')
          selectors <- parseSelectors
          query <- parseFrom
          filter <- parseWhere
          pure $ Select selectors query filter Nothing

    Right (token:q) ->
      lift $ Left ("parsing error: invalid token: " <> (show token))

    Right Nil ->
      lift $ Left "parsing error: empty query"

    Left err ->
      lift $ Left err


parseSelectors :: Clause (List Selector)
parseSelectors = do
  Tuple schema tokens <- get
  case unwrap tokens of
    Right (Comma : (Word w) : (Keyword K.As) : (Word w') : q) -> do
      put $ Tuple schema q
      selectors <- parseSelectors
      pure $ (FieldAs w w') : selectors

    Right (Comma : (Word w) : q) -> do
      put $ Tuple schema q
      selectors <- parseSelectors
      pure $ (Field w) : selectors

    Right (Comma : q) -> do
      put $ Tuple schema q
      pure Nil

    Right _ ->
      pure Nil

    Left err ->
      lift $ Left err


parseFrom :: Clause (Maybe Query)
parseFrom = do
  Tuple schema tokens <- get
  case unwrap tokens of
    Right (Keyword K.From : Parenthesis P.Open : q) -> do
      put $ Tuple schema q
      query <- parse
      Tuple schema' tokens' <- get
      case tokens' of
        (Parenthesis P.Close : q') -> do
          put $ Tuple schema' q'
          pure $ Just query
        _ ->
          lift $ Left "parsing error: unbalanced parenthesis expression"

    Right (Keyword K.From : q) -> do
      lift $ Left "parsing error: FROM clause should be wrapped in parenthesis"

    Right _ ->
      pure Nothing

    Left err ->
      lift $ Left err


parseWhere :: Clause (Maybe Filter)
parseWhere = do
  Tuple schema tokens <- get
  case unwrap tokens of
    Right (Keyword K.Where : q) -> do
      put $ Tuple schema q
      condition <- parseCondition
      pure $ Just (Where condition)

    Right _ ->
      pure Nothing

    Left err ->
      lift $ Left err


parseCondition :: Clause Condition
parseCondition = do
  Tuple schema tokens <- get
  case unwrap tokens of
    Right (Word w : BooleanOp O.Eq : Word w' : q) -> do
      put $ Tuple schema q
      pure $ ConditionTerm (Eq (OpSelector (Field w)) (OpSelector (Field w')))

    Right _ ->
      lift $ Left "not implemented"

    Left err ->
      lift $ Left err


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
