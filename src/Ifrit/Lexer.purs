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
import Data.List(List(..), (:), snoc)
import Data.Maybe(Maybe(..), maybe)
import Data.String(charAt, length)
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
  = Invalid
  | Comma
  | Function Funktion
  | Parenthesis Parenthesis
  | Keyword Keyword
  | Binary Binary
  | Unary Unary
  | Word String
  | Alias String String
  | Boolean Boolean
  | String String
  | Number Decimal
  | EOF

derive instance eqToken :: Eq Token


type Lexer = StateT { pos :: Int, str :: String } (Either String) (List Token)



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
  keyword >>> Keyword </*/> "(AND|AS|FROM|GROUP BY|NULL|OR|SELECT|WHERE)"


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
    Right { result: result, suffix } -> do
      put suffix
      tokens <- tokenize
      pure $ optimize (result : tokens)
    Left { error: ParseError "no match" } -> do
      if pos == length str
        then pure $ EOF : Nil
        else lift $ Left ("invalid token " <> maybe "" show (charAt pos str) <> " at position " <> show pos)
    Left { error: ParseError err } ->
      lift $ Left err


-- NOTE This is done in order to reduce the complexity of future pattern matching.
-- psc gets a bit crazy / slow when trying to figure out all possible matches
-- in a case of looking for the first 3 / 4 elements of a list. This may be moved
-- to the parser directly...
optimize :: List Token -> List Token
optimize =
  let
      optimize' q (Word w : Keyword As : Word as : q') =
        optimize' (snoc q (Alias w as)) q'
      optimize' q (h : q') =
        optimize' (snoc q h) q'
      optimize' q Nil =
        q
  in
      optimize' Nil


-- INSTANCE SHOW
instance showToken :: (Show Number, Show Keyword) => Show Token where
  show (Keyword k) =
    show k
  show (Function f) =
    show f
  show (Word w) =
    w
  show (Alias w as) =
    w <> " AS " <> as
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
  show Invalid =
    "INVALID_TOKEN"
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
