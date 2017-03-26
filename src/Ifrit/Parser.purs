module Ifrit.Parser where

import Prelude

import Control.Monad.State(StateT, get, lift, put)
import Data.Decimal(Decimal, toString)
import Data.Either(Either(..))
import Data.List(List(..), (:))
import Data.Maybe(Maybe(..), maybe)

import Ifrit.Lexer as Lexer


type Parser expr = StateT (List Lexer.Token) (Either String) expr


class Parse expr where
  parse :: Parser expr


data Statement
  = Select (List Selector) (Maybe Statement) (Maybe Condition) (Maybe Index)

derive instance eqStatement :: Eq Statement


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


data Index
  = IdxField String
  | IdxNull

derive instance eqIndex :: Eq Index



-- INSTANCE PARSE
instance parseIndex :: Parse Index where
  parse = do
    tokens <- get
    case tokens of
      Nil ->
      lift $ Left "parsing error (GROUP BY Index): incomplete expression"

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
            lift $ Left "parsing error (GROUP BY Index): unbalanced parenthesis expression"

      (h : _) ->
        lift $ Left $ "parsing error (GROUP BY Index): unexpected token: " <> show h


instance parseOperand :: Parse Operand where
  parse = do
    tokens <- get
    case tokens of
      Nil ->
        lift $ Left "parsing error (WHERE Operand): incomplete expression"

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
            lift $ Left "parsing error (WHERE Operand): unbalanced parenthesis expression"

      (h : _) ->
        lift $ Left $ "parsing error (WHERE Operand): unexpected token: " <> show h


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

      (Lexer.Function f : q) -> do
        put q
        selectors :: List Selector <- parse
        case selectors of
          (Selector w Nothing : Nil) -> do
            tokens' <- get
            case tokens' of
              (Lexer.Keyword Lexer.As : Lexer.Word as : q') -> do
                put q'
                pure $ Function f w (Just as)
              _ ->
                pure $ Function f w Nothing
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


instance parseListSelector :: Parse (List Selector) where
  parse = do
    selector :: Selector <- parse
    tokens <- get
    case tokens of
      (Lexer.Comma : q) -> do
        put q
        selectors :: List Selector <- parse
        pure $ selector : selectors
      _ -> do
        pure $ selector : Nil


--instance parseSelector :: Parse Selector where
--  parse = do
--    tokens <- get
--    case tokens of
--      Nil ->
--        lift $ Left "parsing error (SELECT Selector): incomplete expression"
--
--      (Lexer.Alias w as : Lexer.Comma : q) -> do
--        nextSelector w (Just as) q
--
--      (Lexer.Alias w as : q) -> do
--        put q
--        pure $ Single w (Just as)
--
--      (Lexer.Word w : Lexer.Comma : q) ->
--        nextSelector w Nothing q
--
--      (Lexer.Word w : q) -> do
--        put q
--        pure $ Single w Nothing
--
--      (Lexer.Function f : q) -> do
--        put q
--        selector :: Selector <- parse
--        case selector of
--          Single w _ -> do
--            tokens' <- get
--            case tokens' of
--              (Lexer.Keyword Lexer.As : Lexer.Word as : q') -> do
--                put q'
--                pure $ Function f w (Just as)
--              _ ->
--                pure $ Function f w Nothing
--
--          _ ->
--            lift $ Left $ "parsing error (SELECT Function): invalid argument for function " <> show f
--
--      (Lexer.Parenthesis Lexer.Open : q) -> do
--        put q
--        selector :: Selector <- parse
--        tokens' <- get
--        case tokens' of
--          (Lexer.Parenthesis Lexer.Close : q') -> do
--            put q'
--            pure $ selector
--
--          _ ->
--            lift $ Left "parsing error (SELECT Selector): unbalanced parenthesis expression"
--
--      (h : _) ->
--        lift $ Left $ "parsing error (SELECT Selector): unexpected token: " <> show h


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
        tokens' <- get
        case tokens' of
          (Lexer.EOF : Nil) ->
            pure $ Select selectors statement condition index
          _ ->
            lift $ Left "parsin error (SELECT): invalid tokens after SELECT statement"

      (Lexer.Parenthesis Lexer.Open : q) -> do
        put q
        statement :: Statement <- parse
        tokens' <- get
        case tokens' of
          (Lexer.Parenthesis Lexer.Close : q') -> do
            put q'
            pure statement

          _ ->
            lift $ Left "parsing error (SELECT Selector): unbalanced parenthesis expression"


      (h : _) ->
        lift $ Left $ "parsing error (SELECT): unexpected token: " <> show h



-- INSTANCE SHOW
instance showStatement :: Show Statement where
  show (Select selectors statement condition index) =
    let
        statement' = maybe "" (\x -> " FROM (" <> (show x) <> ")") statement
        condition' = maybe "" (\x -> " WHERE (" <> (show x) <> ")") condition
        index' = maybe "" (\x -> " GROUP BY " <> (show x)) index
    in
        "SELECT " <> (show selectors)
        <> statement'
        <> condition'
        <> index'


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


instance showIndex :: Show Index where
  show (IdxField x) =
    x
  show (IdxNull) =
    "NULL"
