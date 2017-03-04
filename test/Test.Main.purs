
module Test.Main where

import Prelude

import Control.Monad.Aff(Aff)
import Control.Monad.Aff.AVar(AVAR)
import Control.Monad.Eff(Eff)
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Eff.Console(CONSOLE)
import Control.Monad.Eff.Exception(EXCEPTION)
import Control.Monad.State(runStateT)
import Data.Argonaut.Core(Json, toObject)
import Data.Argonaut.Decode(decodeJson)
import Data.Argonaut.Decode.Combinators((.?), (.??))
import Data.Argonaut.Encode(encodeJson)
import Data.Argonaut.Parser(jsonParser)
import Data.Array(foldl)
import Data.Either(Either(..))
import Data.Filterable(filterMap)
import Data.Maybe(Maybe(..))
import Data.Traversable(traverse)
import Data.Tuple(Tuple(..))
import Ifrit.Core(Pipeline, JsonSchema)
import Ifrit.Drivers.MongoDB(ingest)
import Node.Encoding (Encoding(..))
import Node.FS(FS)
import Node.FS.Sync as FS
import Node.Globals(__dirname)
import Node.Path(concat, extname)
import Test.Unit (Test, test, failure, success)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testDir :: String
testDir =
  concat [__dirname, "..", "..", "test", "fixtures"]

maybeSpec :: String -> String -> Maybe String
maybeSpec base file =
  if extname file == ".json"
  then Just (concat [base, file])
  else Nothing

loadFixtures :: forall e. String -> Aff (fs :: FS, err :: EXCEPTION | e) (Either String (Array Json))
loadFixtures dir = liftEff do
  files <- FS.readdir dir
  values <- traverse (FS.readTextFile UTF8) (filterMap (maybeSpec dir) files)
  pure $ traverse jsonParser values


fromFixture :: forall e. Json -> Either String (Test (fs :: FS, err :: EXCEPTION | e))
fromFixture json =
  case toObject json of
    Nothing ->
      Left "invalid provided fixture: not a valid JSON object"

    Just obj ->
      do
        (schema_in :: JsonSchema) <- obj .? "schema_in" >>= decodeJson
        (pipeline_in :: Pipeline) <- obj .? "pipeline_in" >>= decodeJson
        (error :: Maybe String) <- obj .?? "error"
        (pipeline_out :: Maybe Json) <- obj .?? "pipeline_out"
        (schema_out :: Maybe Json) <- obj .?? "schema_out"

        case Tuple error [schema_out, pipeline_out] of
          --
          -- Error Scenarios
          --
          Tuple (Just error') [Nothing, Nothing] ->
            case runStateT (ingest pipeline_in) schema_in of
              Right _ ->
                Left "expected ingest to fail but was successful"
              Left error'' ->
                pure $ Assert.equal error' error''

          --
          -- Successful Scenarios
          --
          Tuple Nothing [Just schema_out', Just pipeline_out'] ->
            case runStateT (ingest pipeline_in) schema_in of
              Right (Tuple pipeline_out'' schema_out'') ->
                pure $ do
                  Assert.equal pipeline_out' pipeline_out''
                  Assert.equal schema_out' (encodeJson schema_out'')
              Left err ->
                Left err

          --
          -- Weirdly Formatted Fixtures
          --
          _ ->
           Left "invalid provided fixture: properties messed up"


main :: Eff
  ( fs :: FS
  , err :: EXCEPTION
  , console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  ) Unit
main =
  let
    mongodbSuite = test "mongodb" do
      fixtures <- loadFixtures (concat [testDir, "mongodb"])
      let tests = fixtures >>= traverse fromFixture
      case tests of
        Left err ->
          failure err
        Right tests' ->
          foldl (\p s -> p *> s) success tests'
  in
    runTest mongodbSuite
