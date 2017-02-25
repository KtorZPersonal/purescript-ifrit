module Ifrit.Core where

import Prelude
import Data.Argonaut.Core(Json, stringify, isString, isBoolean, isNumber)
import Data.Argonaut.Decode(class DecodeJson, decodeJson)
import Data.Argonaut.Encode(class EncodeJson, encodeJson)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))
import Data.StrMap(StrMap, fromFoldable)
import Data.Traversable(traverse)
import Data.Tuple(Tuple(..))
import Ifrit.Decoder(decode2, decode3)

-- TYPES

data Terminal
  = Field String
  | ConstantString String
  | ConstantBoolean Boolean
  | ConstantNumber Number

data Map
  = Project Terminal
  | Inject Terminal Reduce

data Reduce
  = Avg Terminal

data Stage
  = Map (StrMap Map)

type Pipeline =
  Array Stage

-- INSTANCE DECODEJSON

instance decodeJsonTerminal :: DecodeJson String => DecodeJson Terminal where
  decodeJson =
    let
      decoder (Just "field") (Just f) =
        Right $ Field f
      decoder _ _ =
        Left "Invalid Term Operator"
    in
      decode2 "@" "=" decoder


instance decodeJsonReduce :: DecodeJson String => DecodeJson Reduce where
  decodeJson =
    let
      decoder (Just "avg") (Just f) =
        Avg <$> decodeJson f
      decoder _ _ =
        Left "Invalid Reduce Operator"
    in
      decode2 "@" "=" decoder


instance decodeJsonMap :: DecodeJson String => DecodeJson Map where
  decodeJson =
    let
      decoder (Just "inject") (Just src) (Just op) =
        Inject <$> decodeJson src
               <*> decodeJson op
      decoder (Just "field") Nothing (Just f) =
        Field >>> Project <$> decodeJson f
      decoder (Just "constant") Nothing (Just f) | isNumber f =
        ConstantNumber >>> Project <$> decodeJson f
      decoder (Just "constant") Nothing (Just f) | isBoolean f =
        ConstantBoolean >>> Project <$> decodeJson f
      decoder (Just "constant") Nothing (Just f) | isString f =
        ConstantString >>> Project <$> decodeJson f
      decoder _ _ _ =
        Left "Invalid Map Operator"
    in
      decode3 "@" "[]" "=" decoder


instance decodeJsonStage :: (DecodeJson (StrMap Json), DecodeJson String)
  => DecodeJson Stage where
  decodeJson =
    let
      decoder (Just "map") (Just m) =
        Map <$> traverse decodeJson m
      decoder _ _ =
        Left "Invalid Stage"
     in
      decode2 "@" "=" decoder

-- INSTANCE ENCODEJSON

instance encodeJsonTerminal :: EncodeJson (StrMap String) => EncodeJson Terminal where
  encodeJson term =
    let
      encode :: forall a. EncodeJson a => a -> Json
      encode x = encodeJson $ fromFoldable
        [ Tuple "@" (encodeJson "field")
        , Tuple "=" (encodeJson x)
        ]
    in case term of
      Field f -> encode f
      ConstantString c -> encode c
      ConstantBoolean c -> encode c
      ConstantNumber c -> encode c

instance encodeJsonReduce :: (EncodeJson (StrMap String), EncodeJson Terminal) => EncodeJson Reduce where
  encodeJson (Avg t) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "avg")
      , Tuple "=" (encodeJson t)
      ]


instance encodeJsonMap :: (EncodeJson (StrMap String), EncodeJson Terminal)
  => EncodeJson Map where
  encodeJson (Inject t r) =
    encodeJson $ fromFoldable
    [ Tuple "@" (encodeJson "inject")
    , Tuple "[]"(encodeJson t)
    , Tuple "=" (encodeJson r)
    ]
  encodeJson (Project m) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "field")
      , Tuple "=" (encodeJson m)
      ]


instance encodeJsonStage :: (EncodeJson (StrMap String), EncodeJson Map)
  => EncodeJson Stage where
  encodeJson (Map m) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "map")
      , Tuple "=" (encodeJson m)
      ]

-- INSTANCE SHOW

instance showTerminal :: EncodeJson Terminal => Show Terminal where
  show = encodeJson >>> stringify


instance showReduce :: EncodeJson Reduce => Show Reduce where
  show = encodeJson >>> stringify


instance showMap :: EncodeJson Map => Show Map where
  show = encodeJson >>> stringify


instance showStage :: EncodeJson Stage => Show Stage  where
  show = encodeJson >>> stringify
