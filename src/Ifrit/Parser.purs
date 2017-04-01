module Ifrit.Parser
  (class Parse
  , Parser
  , Aggregation(..)
  , Condition(..)
  , Factor(..)
  , Index(..)
  , Operand(..)
  , Order(..)
  , Projection(..)
  , Selector(..)
  , Statement(..)
  , Term(..)
  , parse
  ) where

import Prelude

import Control.Monad.State(StateT, get, lift, put)
import Data.Decimal(Decimal, toString)
import Data.Either(Either(..))
import Data.List(List(..), (:), intercalate, length)
import Data.Maybe(Maybe(..), maybe)

import Ifrit.Lexer as Lexer


type Parser expr = StateT (List Lexer.Token) (Either String) expr


class Parse expr where
  parse :: Parser expr


data Statement
  = Select (List Projection) (Maybe Statement) (Maybe Condition) (List Order)
  | Group Index (List Aggregation) (Maybe Statement) (Maybe Condition) (List Order)

derive instance eqStatement :: Eq Statement


data Projection
  = Projection Selector

derive instance eqProjection :: Eq Projection


data Aggregation
  = Aggregation Selector

derive instance eqAggregation :: Eq Aggregation


data Selector
  = Selector String (Maybe String)
  | Function Lexer.Funktion String (Maybe String)

derive instance eqSelector :: Eq Selector


data Condition
  = Term Term
  | Or Term Term

derive instance eqCondition :: Eq Condition


data Term
  = Factor Factor
  | And Factor Factor

derive instance eqTerm :: Eq Term


data Factor
  = Operand Operand
  | Binary Lexer.Binary Operand Operand

derive instance eqFactor :: Eq Factor


data Operand
  = String String
  | Boolean Boolean
  | Number Decimal
  | Field String
  | Null
  | Condition Condition

derive instance eqOperand :: Eq Operand


data Order
  = OrderAsc String
  | OrderDesc String

derive instance eqOrder :: Eq Order


data Index
  = IdxField String
  | IdxNull

derive instance eqIndex :: Eq Index



-- INSTANCE PARSE
instance parseIndex :: Parse Index where
  parse = do
    tokens <- get
    case tokens of
      (Lexer.Word s : q) -> do
        put q
        pure $ IdxField s

      (Lexer.Keyword Lexer.Null : q) -> do
        put q
        pure $ IdxNull

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        index :: Index <- parse
        tokens' <- get
        case tokens' of
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure $ index
          _ ->
            lift $ Left "parsing error: unbalanced parenthesis expression"

      (h : _) ->
        lift $ Left $ "parsing error: unexpected token: " <> show h

      Nil ->
      lift $ Left "parsing error: incomplete expression"


instance parseOperand :: Parse Operand where
  parse = do
    tokens <- get
    case tokens of
      (Lexer.String s : q) -> do
        put q
        pure $ String s

      (Lexer.Boolean b : q) -> do
        put q
        pure $ Boolean b

      (Lexer.Number n : q) -> do
        put q
        pure $ Number n

      (Lexer.Keyword Lexer.Null : q) -> do
        put q
        pure $ Null

      (Lexer.Word s : q) -> do
        put q
        pure $ Field s

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        condition :: Condition <- parse
        tokens' <- get
        case tokens' of
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure $ Condition condition
          _ ->
            lift $ Left "parsing error: unbalanced parenthesis expression"

      (h : _) ->
        lift $ Left $ "parsing error: unexpected token: " <> show h

      Nil ->
        lift $ Left "parsing error: incomplete expression"


instance parseFactor :: Parse Factor where
  parse = do
    left :: Operand <- parse
    tokens <- get
    case tokens of
      (Lexer.Binary op : q) -> do
        put q
        right :: Operand <- parse
        pure $ Binary op left right

      _ ->
        pure $ Operand left


instance parseTerm :: Parse Term where
  parse = do
    left :: Factor <- parse
    tokens <- get
    case tokens of
      (Lexer.Keyword Lexer.And : q) -> do
        put q
        right :: Factor <- parse
        pure $ And left right

      _ ->
        pure $ Factor left


instance parseCondition :: Parse Condition where
  parse = do
    left :: Term <- parse
    tokens <- get
    case tokens of
      (Lexer.Keyword Lexer.Or : q) -> do
        put q
        right :: Term <- parse
        pure $ Or left right

      _ ->
        pure $ Term left


instance parseOrder :: Parse Order where
  parse = do
    tokens <- get
    case tokens of
      (Lexer.Word s : Lexer.Keyword Lexer.Asc : q) -> do
        put q
        pure $ OrderAsc s

      (Lexer.Word s : Lexer.Keyword Lexer.Desc : q) -> do
        put q
        pure $ OrderDesc s

      (Lexer.Word s : q) -> do
        put q
        pure $ OrderAsc s

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        order :: Order <- parse
        tokens' <- get
        case tokens' of
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure $ order
          _ ->
            lift $ Left "parsing error: unbalanced parenthesis expression"

      (h : _) ->
        lift $ Left $ "parsing error: unexpected token: " <> show h

      Nil ->
        lift $ Left "parsing error: incomplete expression"


instance parseSelector :: Parse Selector where
  parse = do
    tokens <- get
    case tokens of
      (Lexer.Word w : Lexer.Keyword Lexer.As : Lexer.Word as : q) -> do
        put q
        pure $ Selector w (Just as)

      (Lexer.Word w : q) -> do
        put q
        pure $ Selector w Nothing

      (Lexer.Function f : Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        selectors :: List Selector <- parse
        case selectors of
          (Selector w Nothing : Nil) -> do
            tokens' <- get
            case tokens' of
              (Lexer.Parenthesis Lexer.Close : Lexer.Keyword Lexer.As : Lexer.Word as : q') -> do
                put q'
                pure $ Function f w (Just as)

              (Lexer.Parenthesis Lexer.Close : q') -> do
                put q'
                pure $ Function f w Nothing

              _ ->
                lift $ Left $ "parsing error: unbalanced parenthesis expression"
          _ ->
            lift $ Left $ "parsing error: invalid argument(s) for function " <> show f

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        selector :: Selector <- parse
        tokens' <- get
        case tokens' of
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure $ selector

          _ ->
            lift $ Left $ "parsing error: unbalanced parenthesis expression"

      (h : _) -> do
        lift $ Left $ "parsing error: unexpected token: " <> show h

      Nil -> do
        lift $ Left  "parsing error: empty expression"


instance parseListToken :: Parse a => Parse (List a) where
  parse = do
    order :: a <- parse
    tokens <- get
    case tokens of
      (Lexer.Comma : q) -> do
        put q
        orders :: List a <- parse
        pure $ order : orders
      _ -> do
        pure $ order : Nil


maybeParse :: forall a. Parse a => Lexer.Keyword -> Parser (Maybe a)
maybeParse key = do
  tokens <- get
  case tokens of
    (Lexer.Keyword key' : q') | key' == key -> do
      put q'
      res <- parse
      pure $ Just res
    _ ->
      pure $ Nothing


combine :: List Selector -> Maybe Statement -> Maybe Condition -> Maybe Index -> Maybe (List Order) -> Statement
combine selectors statement condition index orders =
  let
      orders' = maybe Nil (\xs -> xs) orders
  in
      case index of
        Nothing ->
          Select (map Projection selectors) statement condition orders'
        Just index' ->
          Group index' (map Aggregation selectors) statement condition orders'


instance parseStatement :: Parse Statement where
  parse = do
    tokens <- get
    case tokens of
      Nil ->
        lift $ Left "parsing error (SELECT): incomplete expression"

      (Lexer.Keyword Lexer.Select : q) -> do
        put q
        selectors :: List Selector <- parse
        statement :: Maybe Statement <- maybeParse Lexer.From
        condition :: Maybe Condition <- maybeParse Lexer.Where
        index :: Maybe Index <- maybeParse Lexer.GroupBy
        orders :: Maybe (List Order) <- maybeParse Lexer.OrderBy
        tokens' <- get
        case tokens' of
          (Lexer.EOF : Nil) -> do
            pure $ combine selectors statement condition index orders
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure $ combine selectors statement condition index orders
          _ ->
            lift $ Left "parsing error: invalid end of input"

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        statement :: Statement <- parse
        pure statement

      (h : _) ->
        lift $ Left $ "parsing error: unexpected token: " <> show h



-- INSTANCE SHOW
instance showStatement :: Show Statement where
  show (Select projections statement condition orders) =
    let
        projections' = "SELECT " <> (intercalate ", " (map show projections))
        statement' = maybe "" (\x -> " FROM (" <> (show x) <> ")") statement
        condition' = maybe "" (\x -> " WHERE (" <> (show x) <> ")") condition
        orders' =
          if length orders == 0
            then ""
            else " ORDER BY " <> (intercalate ", " (map show orders))
    in
        projections' <> statement' <> condition' <> orders'

  show (Group index aggregations statement condition orders) =
    let
        aggregations' = "SELECT " <> (intercalate ", " (map show aggregations))
        statement' = maybe "" (\x -> " FROM (" <> (show x) <> ")") statement
        condition' = maybe "" (\x -> " WHERE (" <> (show x) <> ")") condition
        index' = " GROUP BY " <> (show index)
        orders' =
          if length orders == 0
            then ""
            else " ORDER BY " <> (intercalate ", " (map show orders))
    in
        aggregations' <> statement' <> condition' <> index' <> orders'


instance showProjection :: Show Projection where
  show (Projection selector) =
    show selector


instance showAggregation :: Show Aggregation where
  show (Aggregation selector) =
    show selector


instance showSelector :: Show Selector where
  show (Selector s Nothing) =
    s
  show (Selector s (Just as)) =
    "(" <> s <> " AS " <> as <> ")"
  show (Function f s Nothing) =
    show f <> "(" <> s <> ")"
  show (Function f s (Just as)) =
    "(" <> show f <> "(" <> s <> ")" <> " AS " <> ")"


instance showCondition :: Show Condition where
  show (Term x) =
    show x
  show (Or a b) =
    show a <> " OR " <> show b


instance showTerm :: Show Term where
  show (Factor x) =
    show x
  show (And a b) =
    show a <> " AND " <> show b


instance showFactor :: Show Factor where
  show (Operand x) =
    show x
  show (Binary op a b) =
    show a <> " " <> show op <> " " <> show b


instance showOperand :: Show Operand where
  show (String x) =
    "\"" <> x <> "\""
  show (Boolean x) =
    show x
  show (Number x) =
    toString x
  show (Field x) =
    x
  show Null =
    "NULL"
  show (Condition x) =
    "(" <> show x <> ")"


instance showOrder :: Show Order where
  show (OrderAsc x) =
    (show x) <> " ASC"
  show (OrderDesc x) =
    (show x) <> " DESC"


instance showIndex :: Show Index where
  show (IdxField x) =
    x
  show (IdxNull) =
    "NULL"
