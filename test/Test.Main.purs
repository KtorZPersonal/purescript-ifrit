module Test.Main where

import Prelude

import Control.Monad.Aff(Aff)
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Eff.Exception(EXCEPTION)
import Data.Argonaut.Core(Json, toObject)
import Data.Argonaut.Decode(decodeJson, (.?))
import Data.Argonaut.Encode(encodeJson)
import Data.Argonaut.Parser(jsonParser)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))
import Data.Pair(Pair(..))
import Data.Traversable(traverse)
import Ifrit.Core(Pipeline, JsonSchema)
import Ifrit.Drivers.MongoDB(ingest, ingestType)
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
  let
    sIn :: Maybe (Either String JsonSchema)
    sIn = (\obj -> do
      schema <- obj .? "schema"
      decodeJson schema
    ) <$> toObject input

    sOut :: Maybe (Either String Json)
    sOut = (\obj -> obj .? "schema") <$> toObject output

    pIn :: Maybe (Either String Pipeline)
    pIn = (\obj -> do
      schema <- obj .? "pipeline"
      decodeJson schema
    ) <$> toObject input

    pOut :: Maybe (Either String Json)
    pOut = (\obj -> obj .? "pipeline") <$> toObject output

    assertSchema :: JsonSchema -> Pipeline -> Json -> Test e
    assertSchema sIn pIn sOut =
      case ingestType sIn pIn of
      Left err ->
        failure err
      Right sOut' ->
        Assert.equal sOut $ (encodeJson sOut')
  in

    case { sIn, sOut, pIn, pOut } of
    { sIn: Just (Right sIn), sOut: Just (Right sOut), pIn: Just (Right pIn), pOut: Just (Right pOut) } ->
      do
        assertSchema sIn pIn sOut
        Assert.equal pOut $ (encodeJson $ ingest pIn)
    _ ->
      failure "invalid provided fixture: expecting an object with 'schema' and 'pipeline' properties"


step :: forall eff. String -> TestSuite (fs :: FS, err :: EXCEPTION | eff)
step fixture = test fixture do
  scenario <- load fixture
  run scenario


main = runTest do
  suite "stage :: map" do
    step "single-field-project"
    step "single-constant-project"
    step "avg-nested-array"
