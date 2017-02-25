module Main where

import Prelude
import Data.Either(Either)
import Control.Monad.Eff(Eff)
import Control.Monad.Eff.Console(CONSOLE, logShow)
import Data.Argonaut.Parser(jsonParser)
import Data.Argonaut.Decode(decodeJson)
import Ifrit.Core(Stage)
import Ifrit.Drivers.MongoDB(ingest)

input :: Either String Stage
input =
  jsonParser
  "{\"@\":\"map\",\"=\":{\"amount\":{\"@\":\"inject\",\"[]\":{\"@\":\"field\",\"=\":\"authorizations\"},\"=\":{\"@\":\"avg\",\"=\":{\"@\":\"field\",\"=\":\"amount\"}}}}}"
  --"{\"@\":\"map\",\"=\":{\"amount\":{\"@\":\"field\",\"=\":\"authorizations\"}}}"
  >>= decodeJson

main :: Eff (console :: CONSOLE) Unit
main =
  do logShow $ input
     logShow $ ingest <$> input
