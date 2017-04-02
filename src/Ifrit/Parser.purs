-- | This module provides logic to convert a list of tokens to an AST.
-- |
-- | The AST representation can then be used for semantic analysis and code generation

module Ifrit.Parser
  (class Parse
  , Parser
  , Error(..)
  , Aggregation(..)
  , Condition(..)
  , Factor(..)
  , Index(..)
  , Limit(..)
  , Offset(..)
  , Operand(..)
  , Order(..)
  , Projection(..)
  , Selector(..)
  , Statement(..)
  , Term(..)
  , parse
  ) where

import Prelude

import Control.Alt((<|>))
import Control.Monad.State(StateT, get, lift, put)
import Data.Decimal(Decimal, toString)
import Data.Either(Either(..))
import Data.Int(fromString)
import Data.List(List(..), (:), intercalate, length)
import Data.Maybe(Maybe(..), maybe)

import Ifrit.Lexer as Lexer


-- | A Parser type materializes the progress of the parser.
-- | It holds a list of token associated with their respective position.
-- | It produces an AST for the given parser
type Parser expr = StateT (List { pos :: Int, token :: Lexer.Token }) (Either String) expr


-- | The `Parse` type class refers to parts of the AST which can be parsed from a list of tokens
class Parse expr where
  parse :: Parser expr


-- | A statement refers to the highest element of the tree. So far, only read-type operations are
-- | represented. Also, in order to simplify the code-generation and the semantic analysis,
-- | aggregations and projections are represented as via 2 constructors although they are both
-- | represented by a SELECT in sql.
data Statement
  = Select (List Projection) (Maybe Statement) (Maybe Condition) (List Order) (Maybe Limit) (Maybe Offset)
  | Group Index (List Aggregation) (Maybe Statement) (Maybe Condition) (List Order) (Maybe Limit) (Maybe Offset)


-- | A projection, i.e. a field selected via a SELECT without any GROUP BY
data Projection
  = Projection Selector


-- | An aggregation, i.e. a field selected via a SELECT + GROUP BY
data Aggregation
  = Aggregation Selector


-- | A selector, i.e a field or function selected via a SELECT
data Selector
  = Selector String (Maybe String)
  | Function Lexer.Funktion String (Maybe String)


-- | A boolean condition associated with a WHERE clause
data Condition
  = Term Term
  | Or Term Term


-- | A term of a boolean condition
data Term
  = Factor Factor
  | And Factor Factor


-- | A factor of a term condition
data Factor
  = Operand Operand
  | Condition Condition
  | Unary Lexer.Unary Factor
  | Binary Lexer.Binary Operand Operand


-- | An operand of an operator
data Operand
  = String String
  | Boolean Boolean
  | Number Decimal
  | Field String
  | Null


-- | An ordering field, associated to an ORDER BY clause
data Order
  = OrderAsc String
  | OrderDesc String


-- | A limit, associated to a LIMIT clause
data Limit
  = Limit Int


-- | An offset, associated to an OFFSET clause
data Offset
  = Offset Int


-- | An index, associated to a GROUP BY clause
data Index
  = IdxField String
  | IdxNull


-- | Static representation of an error
data Error
  = ErrParenthesis (List { pos :: Int, token :: Lexer.Token })
  | ErrUnexpectedTokens (List { pos :: Int, token :: Lexer.Token })
  | ErrNotInteger Int Lexer.Keyword
  | ErrInvalidArgument Int Lexer.Funktion
  | ErrEOF


-- Parse the next clause if the given keyword is encountered, does nothing otherwise
maybeParse :: forall a. Parse a => Lexer.Keyword -> Parser (Maybe a)
maybeParse key = do
  tokens <- get
  case tokens of
    ({ token : Lexer.Keyword key', pos: _ } : q') | key' == key -> do
      put q'
      res <- parse
      pure $ Just res
    _ ->
      pure $ Nothing


-- Small helper to combine all different clause into a statement
combine
  :: List Selector
  -> Maybe Statement
  -> Maybe Condition
  -> Maybe Index
  -> Maybe (List Order)
  -> Maybe Limit
  -> Maybe Offset
  -> Statement
combine selectors statement condition index orders limit offset =
  let
      orders' = maybe Nil (\xs -> xs) orders
  in
      case index of
        Nothing ->
          Select (map Projection selectors) statement condition orders' limit offset
        Just index' ->
          Group index' (map Aggregation selectors) statement condition orders' limit offset


instance parseIndex :: Parse Index where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Word s, pos: _ } : q) -> do
        put q
        pure $ IdxField s

      ({ token: Lexer.Keyword Lexer.Null, pos: _ } : q) -> do
        put q
        pure $ IdxNull

      ({ token: Lexer.Parenthesis Lexer.Open, pos: _ } : q) -> do
        put q
        index :: Index <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close, pos: _ } : q') -> do
            put q'
            pure $ index
          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseOperand :: Parse Operand where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.String s , pos: _ }: q) -> do
        put q
        pure $ String s

      ({ token: Lexer.Boolean b , pos: _ }: q) -> do
        put q
        pure $ Boolean b

      ({ token: Lexer.Number n , pos: _ }: q) -> do
        put q
        pure $ Number n

      ({ token: Lexer.Keyword Lexer.Null , pos: _ }: q) -> do
        put q
        pure $ Null

      ({ token: Lexer.Word s , pos: _ }: q) -> do
        put q
        pure $ Field s

      ({ token: Lexer.Parenthesis Lexer.Open , pos: _ }: q) -> do
        put q
        operand :: Operand <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close , pos: _ }: q') -> do
            put q'
            pure $ operand
          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseFactor :: Parse Factor where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Unary op, pos: _ } : q) -> do
        put q
        factor :: Factor <- parse
        pure $ Unary op factor

      ({ token: Lexer.Parenthesis Lexer.Open, pos: _ } : q) -> do
        put q
        condition :: Condition <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close, pos: _ } : q') -> do
            put q'
            pure $ Condition condition
          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      _ -> do
        left :: Operand <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Binary op, pos: _ } : q) -> do
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
      ({ token: Lexer.Keyword Lexer.And, pos: _ } : q) -> do
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
      ({ token: Lexer.Keyword Lexer.Or, pos: _ } : q) -> do
        put q
        right :: Term <- parse
        pure $ Or left right

      _ ->
        pure $ Term left


instance parseOrder :: Parse Order where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Word s, pos: _ } : { token: Lexer.Keyword Lexer.Asc, pos: _ }: q) -> do
        put q
        pure $ OrderAsc s

      ({ token: Lexer.Word s, pos: _ } : { token: Lexer.Keyword Lexer.Desc, pos: _ }: q) -> do
        put q
        pure $ OrderDesc s

      ({ token: Lexer.Word s, pos: _ } : q) -> do
        put q
        pure $ OrderAsc s

      ({ token: Lexer.Parenthesis Lexer.Open, pos: _ }: q) -> do
        put q
        order :: Order <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close, pos: _ }: q') -> do
            put q'
            pure $ order

          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseLimit :: Parse Limit where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Number n, pos } : q) -> do
        case fromString $ toString $ n of
          Just i -> do
            put q
            pure $ Limit i
          Nothing ->
            lift $ Left $ show $ ErrNotInteger pos Lexer.Limit
      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseOffset :: Parse Offset where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Number n, pos } : q) -> do
        case fromString $ toString $ n of
          Just i -> do
            put q
            pure $ Offset i
          Nothing ->
            lift $ Left $ show $ ErrNotInteger pos Lexer.Offset
      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseSelector :: Parse Selector where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Word w, pos: _ } : { token: Lexer.Keyword Lexer.As, pos: _ } : { token: Lexer.Word as , pos: _ }: q) -> do
        put q
        pure $ Selector w (Just as)

      ({ token: Lexer.Word w , pos: _ }: q) -> do
        put q
        pure $ Selector w Nothing

      ({ token: Lexer.Function f, pos: _ } : { token: Lexer.Parenthesis Lexer.Open , pos } : q) -> do
        put q
        selectors :: List Selector <- parse
        case selectors of
          (Selector w Nothing : Nil) -> do
            tokens' <- get
            case tokens' of
              ({ token: Lexer.Parenthesis Lexer.Close, pos: _ } : { token: Lexer.Keyword Lexer.As, pos: _ } : { token: Lexer.Word as , pos: _ }: q') -> do
                put q'
                pure $ Function f w (Just as)

              ({ token: Lexer.Parenthesis Lexer.Close , pos: _ }: q') -> do
                put q'
                pure $ Function f w Nothing

              q' ->
                lift $ Left $ show $ ErrParenthesis q'

          _ ->
            lift $ Left $ show $ ErrInvalidArgument pos f

      ({ token: Lexer.Parenthesis Lexer.Open , pos: _ }: q) -> do
        put q
        selector :: Selector <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close , pos: _ }: q') -> do
            put q'
            pure $ selector

          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance parseList :: Parse a => Parse (List a) where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Parenthesis Lexer.Open , pos: _ } : q) -> do
        put q
        xs :: List a <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Parenthesis Lexer.Close, pos: _ } : { token: Lexer.Comma, pos: _ } : q') -> do
            put q'
            xs' :: List a <- parse
            pure $ xs <|> xs'

          ({ token: Lexer.Parenthesis Lexer.Close , pos: _ } : q') -> do
            put q'
            pure $ xs

          q' ->
            lift $ Left $ show $ ErrParenthesis q'

      _ -> do
        x :: a <- parse
        tokens' <- get
        case tokens' of
          ({ token: Lexer.Comma, pos: _ } : q) -> do
            put q
            xs :: List a <- parse
            pure $ x : xs

          _ -> do
            pure $ x : Nil


instance parseStatement :: Parse Statement where
  parse = do
    tokens <- get
    case tokens of
      ({ token: Lexer.Keyword Lexer.Select, pos: _ } : q) -> do
        put q
        select :: List Selector <- parse
        from :: Maybe Statement <- maybeParse Lexer.From
        where_ :: Maybe Condition <- maybeParse Lexer.Where
        groupBy :: Maybe Index <- maybeParse Lexer.GroupBy
        orderBy :: Maybe (List Order) <- maybeParse Lexer.OrderBy
        limit :: Maybe Limit <- maybeParse Lexer.Limit
        offset :: Maybe Offset <- maybeParse Lexer.Offset
        tokens' <- get
        case tokens' of
          ({ token: Lexer.EOF, pos: _ } : Nil) -> do
            pure $ combine select from where_ groupBy orderBy limit offset
          ({ token: Lexer.Parenthesis Lexer.Close, pos: _ } : q') -> do
            put q'
            pure $ combine select from where_ groupBy orderBy limit offset
          _ ->
            lift $ Left $ show $ ErrEOF

      ({ token: Lexer.Parenthesis Lexer.Open, pos: _ } : q) -> do
        put q
        select :: Statement <- parse
        pure select

      q ->
        lift $ Left $ show $ ErrUnexpectedTokens q


instance showStatement :: Show Statement where
  show (Select projections statement condition orders limit offset) =
    let
        projections' = "SELECT " <> (intercalate ", " (map show projections))
        statement' = maybe "" (\x -> " FROM (" <> (show x) <> ")") statement
        condition' = maybe "" (\x -> " WHERE (" <> (show x) <> ")") condition
        orders' =
          if length orders == 0
            then ""
            else " ORDER BY " <> (intercalate ", " (map show orders))
        limit' = maybe "" (\x -> " LIMIT " <> show x ) limit
        offset' = maybe "" (\x -> " OFFSET " <> show x ) offset
    in
        projections' <> statement' <> condition' <> orders' <> limit' <> offset'

  show (Group index aggregations statement condition orders limit offset) =
    let
        aggregations' = "SELECT " <> (intercalate ", " (map show aggregations))
        statement' = maybe "" (\x -> " FROM (" <> (show x) <> ")") statement
        condition' = maybe "" (\x -> " WHERE (" <> (show x) <> ")") condition
        index' = " GROUP BY " <> (show index)
        orders' =
          if length orders == 0
            then ""
            else " ORDER BY " <> (intercalate ", " (map show orders))
        limit' = maybe "" (\x -> " LIMIT " <> show x ) limit
        offset' = maybe "" (\x -> " OFFSET " <> show x ) offset
    in
        aggregations' <> statement' <> condition' <> index' <> orders' <> limit' <> offset'


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
  show (Condition x) =
    "(" <> show x <> ")"
  show (Unary op x) =
    show op <> "(" <> show x <> ")"
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


instance showOrder :: Show Order where
  show (OrderAsc x) =
    (show x) <> " ASC"
  show (OrderDesc x) =
    (show x) <> " DESC"


instance showLimit :: Show Limit where
  show (Limit i) =
    show i


instance showOffset :: Show Offset where
  show (Offset i) =
    show i


instance showIndex :: Show Index where
  show (IdxField x) =
    x
  show (IdxNull) =
    "NULL"


instance showError :: Show Error where
  show err =
    case err of
      ErrParenthesis ({ token, pos } : _) ->
        "unbalanced parenthesis expression: expected `)` but got: "
          <> show token <> " at position " <> show pos

      ErrParenthesis Nil ->
        "unbalanced parenthesis expression: expected `)` but got end of input"

      ErrUnexpectedTokens ({ token, pos } : _) ->
        "unexpected token: " <> show token <> " at position " <> show pos

      ErrUnexpectedTokens Nil ->
        "unexpected end of expression"

      ErrNotInteger pos k ->
        show k <> " must be an integer at position " <> show pos

      ErrInvalidArgument pos f ->
        show f <> " has an invalid argument at position " <> show pos

      ErrEOF ->
        "unexpected end of input"


derive instance eqStatement :: Eq Statement


derive instance eqProjection :: Eq Projection


derive instance eqAggregation :: Eq Aggregation


derive instance eqSelector :: Eq Selector


derive instance eqCondition :: Eq Condition


derive instance eqTerm :: Eq Term


derive instance eqFactor :: Eq Factor


derive instance eqOperand :: Eq Operand


derive instance eqOrder :: Eq Order


derive instance eqLimit :: Eq Limit


derive instance eqOffset :: Eq Offset


derive instance eqIndex :: Eq Index
