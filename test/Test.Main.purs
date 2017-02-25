module Test.Main where

import Prelude

import Control.Monad.Aff(Aff)
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Eff.Exception(EXCEPTION)
import Data.Argonaut.Core(Json)
import Data.Argonaut.Decode(decodeJson)
import Data.Argonaut.Encode(encodeJson)
import Data.Argonaut.Parser(jsonParser)
import Data.Either(Either(..))
import Data.Pair(Pair(..))
import Data.Traversable(traverse)
import Ifrit.Core(Pipeline)
import Ifrit.Drivers.MongoDB(ingest)
import Node.Encoding (Encoding(..))
import Node.FS(FS)
import Node.Path(concat)
import Node.Globals(__dirname)
import Node.FS.Sync as FS
import Test.Unit (Test, TestSuite, suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

load :: forall eff.
  String
  -> Aff (fs :: FS, err :: EXCEPTION | eff) (Either String (Pair Json))
load fixture =
  let
    files = Pair <$> FS.readTextFile UTF8 (concat [__dirname, "..", "..", "test", "fixtures", fixture, "in.json"])
                 <*> FS.readTextFile UTF8 (concat [__dirname, "..", "..", "test", "fixtures", fixture, "out.json"])
  in
    liftEff (traverse jsonParser <$> files)


run :: forall e. Either String (Pair Json) -> Test e
run (Left e) =
  failure e
run (Right (Pair input output)) =
  case (decodeJson input :: Either String Pipeline) of
    Left e ->
      failure e
    Right pipeline ->
      Assert.equal output $ (encodeJson $ ingest pipeline)


step :: forall eff. String -> TestSuite (fs :: FS, err :: EXCEPTION | eff)
step fixture = test fixture do
  scenario <- load fixture
  run scenario


main = runTest do
  suite "stage :: map" do
    step "single-field-project"
    step "single-constant-project"
    step "avg-nested-array"
