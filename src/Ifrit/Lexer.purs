module Ifrit.Lexer
  ( Binary(..)
  , Funktion(..)
  , Keyword(..)
  , Lexer
  , Parenthesis(..)
  , Token(..)
  , Unary(..)
  , tokenize
  ) where

import Prelude

import Control.Alt((<|>))
import Control.Monad.State(StateT, get, lift, put)
import Data.Decimal(Decimal, fromString, toString)
import Data.Either(Either(..))
import Data.List(List(..), (:))
import Data.Maybe(Maybe(..), maybe)
import Data.String(charAt, length)
import Data.String.Regex(replace)
import Data.String.Regex.Flags(global)
import Data.String.Regex.Unsafe(unsafeRegex)
import Partial.Unsafe(unsafePartial)
import Text.Parsing.StringParser(Parser, ParseError(..), unParser, fail)
import Text.Parsing.StringParser.String(regex)


-- TYPES
type Lexer = StateT { pos :: Int, str :: String } (Either String) (List { pos:: Int, token:: Token })


data Keyword
  = And
  | As
  | Asc
  | Desc
  | Distinct
  | From
  | GroupBy
  | Limit
  | Null
  | Offset
  | Or
  | OrderBy
  | Select
  | Where

derive instance eqKeyword :: Eq Keyword


data Parenthesis
  = Close
  | Open

derive instance eqParenthesis :: Eq Parenthesis


data Funktion
  = Avg
  | Count
  | Max
  | Min
  | Sum

derive instance eqFunktion :: Eq Funktion


data Binary
  = Eq
  | Neq
  | Lt
  | Gt

derive instance eqBinary :: Eq Binary


data Unary
  = Not

derive instance eqUnary :: Eq Unary


data Token
  = Comma
  | Function Funktion
  | Parenthesis Parenthesis
  | Keyword Keyword
  | Binary Binary
  | Unary Unary
  | Word String
  | Boolean Boolean
  | String String
  | Number Decimal
  | EOF

derive instance eqToken :: Eq Token


-- ERRORS
data Error
  = ErrInvalidToken String Int

instance showError :: Show Error where
  show err =
    case err of
      ErrInvalidToken str pos ->
        "invalid token " <> maybe "" show (charAt pos str) <> " at position " <> show pos


-- UTILS
keyword :: String -> Keyword
keyword str = unsafePartial $
  case trim str of
    "AND" -> And
    "AS" -> As
    "ASC" -> Asc
    "DESC" -> Desc
    "DISTINCT" -> Distinct
    "FROM" -> From
    "GROUPBY" -> GroupBy
    "LIMIT" -> Limit
    "NULL" -> Null
    "OFFSET" -> Offset
    "OR" -> Or
    "ORDERBY" -> OrderBy
    "SELECT" -> Select
    "WHERE" -> Where


function :: String -> Funktion
function str = unsafePartial $
  case trim str of
    "AVG" -> Avg
    "COUNT" -> Count
    "MAX" -> Max
    "MIN" -> Min
    "SUM" -> Sum


trim :: String -> String
trim str =
  replace (unsafeRegex "\\s" global) "" str


unquote :: String -> String
unquote str =
  replace (unsafeRegex "\"" global) "" str


parse :: (String -> Token) -> String -> Parser Token
parse f s =
  f <$> regex ("\\s*" <> s <> "\\s*")


parse' :: Token -> String -> Parser Token
parse' t s =
  parse (\_ -> t) s


infixr 7 parse' as </$/>
infixr 7 parse as </*/>


nextKeyword :: Parser Token
nextKeyword =
  keyword >>> Keyword </*/>
    -- NOTE Careful about the order here, OR is included in ORDER BY, AS in ASC etc...
    "(DISTINCT|GROUP BY|ORDER BY|OFFSET|SELECT|WHERE|LIMIT|NULL|FROM|WHERE|AND|ASC|AS|OR|DESC)"


nextFunction :: Parser Token
nextFunction =
  function >>> Function </*/> "(AVG|COUNT|MAX|MIN|SUM)"


nextUnary :: Parser Token
nextUnary =
  Unary Not </$/> "NOT"


nextBinary :: Parser Token
nextBinary =
  Binary Neq </$/> "!="
  <|> Binary Eq </$/> "="
  <|> Binary Lt </$/> "<"
  <|> Binary Gt </$/> ">"


nextBoolean :: Parser Token
nextBoolean =
  let
      fromString' "true" =
        true
      fromString' _ =
        false
  in
    trim >>> fromString' >>> Boolean </*/> "(true|false)"


nextNumber :: Parser Token
nextNumber =
  let
      fromString' str =
        case fromString (trim str) of
          Just res ->
            pure $ Number res
          Nothing ->
            fail "invalid number"
   in
      regex ("\\s*([0-9]*\\.?[0-9]+)\\s*") >>= fromString'


nextString :: Parser Token
nextString =
  trim >>> unquote >>> String </*/> "\"([a-zA-Z0-9_.]+)\""


nextWord :: Parser Token
nextWord =
  trim >>> Word </*/> "([a-zA-Z0-9_.]+)"


nextParenthesis :: Parser Token
nextParenthesis =
  Parenthesis Close </$/> "\\)"
  <|> Parenthesis Open </$/> "\\("


nextComma :: Parser Token
nextComma =
  Comma </$/> ","


parser :: Parser Token
parser =
  nextKeyword
  <|> nextFunction
  <|> nextUnary
  <|> nextBinary
  <|> nextBoolean
  <|> nextNumber
  <|> nextString
  <|> nextWord
  <|> nextParenthesis
  <|> nextComma


-- EXPORTS
tokenize :: Lexer
tokenize = do
  { pos, str } <- get
  case unParser parser { pos, str } of
    Right { result: token, suffix } -> do
      put suffix
      tokens <- tokenize
      pure $ { pos, token } : tokens
    Left { error: ParseError "no match" } -> do
      if pos == length str
         then pure $ { pos, token: EOF } : Nil
        else lift $ Left $ show $ ErrInvalidToken str pos
    Left { error: ParseError err } ->
      lift $ Left err


-- INSTANCE SHOW
instance showToken :: (Show Number, Show Keyword) => Show Token where
  show (Keyword k) =
    show k
  show (Function f) =
    show f
  show (Word w) =
    w
  show (String s) =
    "\"" <> s <> "\""
  show (Boolean b) =
    show b
  show (Number n) =
    toString n
  show Comma =
    ","
  show (Parenthesis Open) =
    "("
  show (Parenthesis Close) =
    ")"
  show (Binary x) =
    show x
  show (Unary x) =
    show x
  show EOF =
    "EOF"


instance showBinary :: Show Binary where
  show Eq =
    "="
  show Neq =
    "!="
  show Lt =
    "<"
  show Gt =
    ">"


instance showUnary :: Show Unary where
  show Not =
    "NOT"


instance showKeyword :: Show Keyword where
  show And =
    "AND"
  show As =
    "AS"
  show Asc =
    "ASC"
  show Desc =
    "DESC"
  show Distinct =
    "DISTINCT"
  show From =
    "FROM"
  show GroupBy =
    "GROUP BY"
  show Limit =
    "LIMIT"
  show Null =
    "NULL"
  show Offset =
    "OFFSET"
  show Or =
    "OR"
  show OrderBy =
    "ORDER BY"
  show Select =
    "SELECT"
  show Where =
    "WHERE"


instance showFunktion :: Show Funktion where
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
