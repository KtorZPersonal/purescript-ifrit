module Ifrit.Lexer
  ( Keyword(..)
  , Token(..)
  , Parenthesis(..)
  , Binary(..)
  , Unary(..)
  , Lexer
  , tokenize
  ) where

import Prelude

import Control.Alt((<|>))
import Control.Monad.State(StateT, get, lift, put)
import Data.Decimal(Decimal, fromString)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))
import Data.String.Regex(replace)
import Data.String.Regex.Flags(global)
import Data.String.Regex.Unsafe(unsafeRegex)
import Partial.Unsafe(unsafePartial)
import Text.Parsing.StringParser(Parser, ParseError(..), unParser, fail)
import Text.Parsing.StringParser.String(regex)

-- TYPES
data Keyword
  = And
  | As
  | From
  | GroupBy
  | Null
  | Or
  | Select
  | Where

data Parenthesis
  = Close
  | Open

data Binary
  = Eq
  | Neq
  | Lt
  | Gt

data Unary
  = Not

data Token
  = Invalid
  | Comma
  | Parenthesis Parenthesis
  | Keyword Keyword
  | Binary Binary
  | Unary Unary
  | Word String
  | Boolean Boolean
  | String String
  | Number Decimal

type Lexer = StateT { pos :: Int, str :: String } (Either String) (Array Token)


-- UTILS
keyword :: String -> Keyword
keyword str = unsafePartial $
  case trim str of
    "AND" -> And
    "AS" -> As
    "FROM" -> From
    "GROUPBY" -> GroupBy
    "NULL" -> Null
    "OR" -> Or
    "SELECT" -> Select
    "WHERE" -> Where

trim :: String -> String
trim str =
  replace (unsafeRegex "\\s" global) "" str

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
  keyword >>> Keyword </*/> "(AND|AS|FROM|GROUP BY|NULL|OR|SELECT|WHERE)"

nextUnary :: Parser Token
nextUnary =
  Unary Not </$/> "NOT"

nextBinary :: Parser Token
nextBinary =
  Binary Eq </$/> "=="
  <|> Binary Neq </$/> "/="
  <|> Binary Lt </$/> "<"
  <|> Binary Gt </$/> ">"

nextParenthesis :: Parser Token
nextParenthesis =
  Parenthesis Close </$/> "\\)"
  <|> Parenthesis Open </$/> "\\("

nextComma :: Parser Token
nextComma =
  Comma </$/> ","

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
        case fromString str of
          Just res ->
            pure $ Number res
          Nothing ->
            fail "invalid number"
   in
      regex ("\\s*[0-9]*\\.?[0-9]+\\s*") >>= fromString'

nextString :: Parser Token
nextString =
  trim >>> String </*/> "\"[a-zA-Z0-9_.]+\""

nextWord :: Parser Token
nextWord =
  trim >>> Word </*/> "[a-zA-Z0-9_.]+"

nextInvalid :: Parser Token
nextInvalid =
  Invalid </$/> "[^\\s]"


parser :: Parser Token
parser =
  nextKeyword
  <|> nextUnary
  <|> nextParenthesis
  <|> nextBinary
  <|> nextComma
  <|> nextBoolean
  <|> nextNumber
  <|> nextString
  <|> nextWord
  <|> nextInvalid


-- EXPORTS
tokenize :: Lexer
tokenize = do
  { pos, str } <- get
  case unParser parser { pos, str } of
    Right { result: Invalid, suffix } -> do
      lift $ Left ("invalid token at position " <> show pos)
    Right { result: result, suffix } -> do
      put suffix
      tokens <- tokenize
      pure $ [result] <|> tokens
    Left { error: ParseError "no match" } -> do
      pure $ []
    Left { error: ParseError err } ->
      lift $ Left err


-- INSTANCES
instance showToken :: (Show Number, Show Keyword) => Show Token where
  show (Keyword k) =
    show k
  show (Word w) =
    w
  show (String s) =
    "\"" <> s <> "\""
  show (Boolean b) =
    show b
  show (Number n) =
    show n
  show Comma =
    "COMMA"
  show (Parenthesis Open) =
    "PARENO"
  show (Parenthesis Close) =
    "PARENC"
  show (Binary Eq) =
    "=="
  show (Binary Neq) =
    "/="
  show (Binary Lt) =
    "<"
  show (Binary Gt) =
    ">"
  show (Unary Not) =
    "NOT"
  show Invalid =
    "INVALID_TOKEN"

instance showKeyword :: Show Keyword where
  show Select =
    "SELECT"
  show GroupBy =
    "GROUP BY"
  show Where =
    "WHERE"
  show As =
    "AS"
  show From =
    "FROM"
  show And =
    "AND"
  show Or =
    "OR"
  show Null =
    "NULL"
