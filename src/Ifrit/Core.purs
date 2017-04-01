module Ifrit.Core where

import Prelude

import Control.Monad.State(evalStateT)
import Data.Argonaut.Core(Json)
import Data.Either(Either)

import Ifrit.Lexer as Lexer
import Ifrit.Parser as Parser
import Ifrit.Semantic as Semantic
import Ifrit.Driver.MongoDB as MongoDB

class Compile driver where
  compile :: Json -> String -> Either String driver

newtype MongoDB = MongoDB Json

instance showMongoDB :: Show MongoDB where
  show (MongoDB json) =
    show json

instance compileMongoDB :: Compile MongoDB where
  compile inputSchema input = do
    schemaIn <- Semantic.fromJson inputSchema
    tokens <- evalStateT Lexer.tokenize { pos: 0, str: input }
    ast :: Parser.Statement <- evalStateT Parser.parse tokens
    schema <- Semantic.analyze schemaIn ast
    output <- MongoDB.ingest ast
    pure $ MongoDB output
