module Ifrit.Lexer
  ( Keyword(..)
  , Token(..)
  , Parenthesis(..)
  , BooleanOp(..)
  , Lexer
  , tokenize
  ) where

import Prelude

import Control.Alt((<|>))
import Control.Monad.State(StateT, get, lift, put)
import Data.Either(Either(..))
import Data.String.Regex(replace)
import Data.String.Regex.Flags(global)
import Data.String.Regex.Unsafe(unsafeRegex)
import Partial.Unsafe(unsafePartial)
import Text.Parsing.StringParser(Parser, ParseError(..), unParser)
import Text.Parsing.StringParser.String(regex)


-- TYPES
data Keyword
  = As
  | Select
  | From
  | GroupBy
  | Where

data Parenthesis
  = Close
  | Open

data BooleanOp
  = Eq
  | Neq
  | Lt
  | Gt

data Token
  = Comma
  | Invalid
  | Keyword Keyword
  | Null
  | Parenthesis Parenthesis
  | BooleanOp BooleanOp
  | Word String

type Lexer = StateT { pos :: Int, str :: String } (Either String) (Array Token)


-- UTILS
keyword :: String -> Keyword
keyword str = unsafePartial $
  case trim str of
    "SELECT" -> Select
    "WHERE" -> Where
    "FROM" -> From
    "AS" -> As
    "GROUPBY" -> GroupBy

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
  keyword >>> Keyword </*/> "(SELECT|WHERE|FROM|AS|GROUP BY)"

nextNull :: Parser Token
nextNull =
  Null </$/> "null"

nextParenthesis :: Parser Token
nextParenthesis =
  Parenthesis Close </$/> "\\)"
  <|> Parenthesis Open </$/> "\\("

nextBooleanOp :: Parser Token
nextBooleanOp =
  BooleanOp Eq </$/> "=="
  <|> BooleanOp Neq </$/> "/="
  <|> BooleanOp Lt </$/> "<"
  <|> BooleanOp Gt </$/> ">"

nextComma :: Parser Token
nextComma =
  Comma </$/> ","

nextWord :: Parser Token
nextWord =
  trim >>> Word </*/> "[a-zA-Z0-9_.]+"

nextInvalid :: Parser Token
nextInvalid =
  Invalid </$/> "[^\\s]"


parser :: Parser Token
parser =
  nextKeyword
  <|> nextNull
  <|> nextParenthesis
  <|> nextBooleanOp
  <|> nextComma
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
  show Null =
    "null"
  show Comma =
    "`,`"
  show (Parenthesis Open) =
    "("
  show (Parenthesis Close) =
    ")"
  show (BooleanOp Eq) =
    "=="
  show (BooleanOp Neq) =
    "/="
  show (BooleanOp Lt) =
    "<"
  show (BooleanOp Gt) =
    ">"
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
