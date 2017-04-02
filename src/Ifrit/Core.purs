-- | This module defines high-level instances to handle compilation with Ifrit

module Ifrit.Core where

import Prelude

import Control.Monad.State(evalStateT)
import Data.Argonaut.Core(Json)
import Data.Either(Either)

import Ifrit.Driver.MongoDB as MongoDB
import Ifrit.Lexer as Lexer
import Ifrit.Parser as Parser
import Ifrit.Semantic as Semantic


-- | The `Compile` type class represents target NoSQL drivers.
-- |
-- | Each driver may have a different input and output type representation
class Compile driver where
  compile :: Json -> String -> Either String driver

-- | MongoDB driver type: compiles SQL to an aggregation pipeline as JSON
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
