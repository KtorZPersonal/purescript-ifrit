module Ifrit.Core where

import Prelude

import Data.Argonaut.Core(Json, stringify, isArray, isString, isBoolean, isNumber, foldJson)
import Data.Argonaut.Decode(class DecodeJson, decodeJson)
import Data.Argonaut.Encode(class EncodeJson, encodeJson)
import Data.Array(concat, head, length, snoc)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..), maybe)
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
  | Add (Array Terminal)
  | Abs Terminal
  | Ceil Terminal
  | Div (Array Terminal)
  | Floor Terminal
  | Mult (Array Terminal)

data Reduce
  = Avg Terminal
  | Min Terminal
  | Max Terminal
  | Sum Terminal

data Stage
  = Map (StrMap Map)
  | Reduce (Maybe Terminal) (StrMap Reduce)

type Pipeline =
  Array Stage

data JsonSchema
  = JObject (StrMap JsonSchema)
  | JArray JsonSchema
  | JString
  | JNumber
  | JBoolean


-- INSTANCE DECODEJSON
instance decodeJsonTerminal :: DecodeJson String => DecodeJson Terminal where
  decodeJson =
    let
        decoder (Just "field") (Just f) =
          Field <$> decodeJson f
        decoder (Just "constant") (Just c) | isNumber c =
          ConstantNumber <$> decodeJson c
        decoder (Just "constant") (Just c) | isBoolean c =
          ConstantBoolean <$> decodeJson c
        decoder (Just "constant") (Just c) | isString c =
          ConstantString <$> decodeJson c
        decoder _ _ =
          Left "unknown terminal operator"
    in
        decode2 "@" "=" decoder


instance decodeJsonReduce :: DecodeJson String => DecodeJson Reduce where
  decodeJson =
    let
        decoder (Just "avg") (Just f) =
          Avg <$> decodeJson f
        decoder (Just "max") (Just f) =
          Max <$> decodeJson f
        decoder (Just "min") (Just f) =
          Min <$> decodeJson f
        decoder (Just "sum") (Just f) =
          Sum <$> decodeJson f
        decoder _ _ =
          Left "unknown reduce operator"
    in
        decode2 "@" "=" decoder


instance decodeJsonMap :: DecodeJson String => DecodeJson Map where
  decodeJson =
    let
        decoder (Just "abs") Nothing (Just term) =
          Abs <$> decodeJson term
        decoder (Just "add") Nothing (Just terms) | isArray terms =
          Add <$> decodeJson terms
        decoder (Just "ceil") Nothing (Just term) =
          Ceil <$> decodeJson term
        decoder (Just "constant") Nothing (Just c) | isNumber c =
          ConstantNumber >>> Project <$> decodeJson c
        decoder (Just "constant") Nothing (Just c) | isBoolean c =
          ConstantBoolean >>> Project <$> decodeJson c
        decoder (Just "constant") Nothing (Just c) | isString c =
          ConstantString >>> Project <$> decodeJson c
        decoder (Just "div") Nothing (Just terms) =
          let
              length2 xs =
                if length xs /= 2
                then Left "invalid operation @div: target `=` should be a list of two elements"
                else pure $ Div xs
          in
              decodeJson terms >>= length2
        decoder (Just "field") Nothing (Just f) =
          Field >>> Project <$> decodeJson f
        decoder (Just "floor") Nothing (Just term) =
          Floor <$> decodeJson term
        decoder (Just "inject") (Just src) (Just op) =
          Inject <$> decodeJson src
                 <*> decodeJson op
        decoder (Just "mult") Nothing (Just terms) | isArray terms =
          Mult <$> decodeJson terms
        decoder _ _ _ =
          Left "unknown map operator"
    in
        decode3 "@" "[]" "=" decoder


instance decodeJsonStage :: (DecodeJson (StrMap Json), DecodeJson String)
  => DecodeJson Stage where
  decodeJson =
    let
        decoder (Just "map") (Just m) Nothing =
          Map <$> traverse decodeJson m
        decoder (Just "reduce") (Just m) Nothing =
          Reduce <$> Right Nothing
                 <*> traverse decodeJson m
        decoder (Just "reduce") (Just m) (Just i) =
          Reduce <$> decodeJson i
                 <*> traverse decodeJson m
        decoder _ _ _ =
          Left "unknown stage operator"
     in
        decode3 "@" "=" "#" decoder


instance decodeJsonSchema :: DecodeJson JsonSchema where
  decodeJson =
    let
        decodeNull _ =
          Left "can't decode null to schema"
        decodeBoolean _ =
          Left "can't decode boolean to schema"
        decodeNumber _ =
          Left "can't decode number to schema"
        decodeString "string" =
          Right JString
        decodeString "number" =
          Right JNumber
        decodeString "boolean" =
          Right JBoolean
        decodeString s =
          Left ("can't decode type: invalid provided type: " <> s)
        decodeArray xs =
          if length xs /= 1
          then Left "can't decode array: exactly one element is expected"
          else case head xs of
            Nothing ->
              Left "can't decode array: exactly one element is expected"
            Just schema ->
              JArray <$> decodeJson schema
        decodeObject obj = JObject <$> traverse decodeJson obj
    in
        foldJson decodeNull decodeBoolean decodeNumber decodeString decodeArray decodeObject


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
  encodeJson (Min t) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "min")
      , Tuple "=" (encodeJson t)
      ]
  encodeJson (Max t) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "max")
      , Tuple "=" (encodeJson t)
      ]
  encodeJson (Sum t) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "sum")
      , Tuple "=" (encodeJson t)
      ]


instance encodeJsonMap :: (EncodeJson (StrMap String), EncodeJson Terminal)
  => EncodeJson Map where
  encodeJson (Abs term) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "abs")
      , Tuple "=" (encodeJson term)
      ]
  encodeJson (Add terms) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "add")
      , Tuple "=" (encodeJson terms)
      ]
  encodeJson (Ceil term) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "ceil")
      , Tuple "=" (encodeJson term)
      ]
  encodeJson (Div terms) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "div")
      , Tuple "=" (encodeJson terms)
      ]
  encodeJson (Floor term) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "floor")
      , Tuple "=" (encodeJson term)
      ]
  encodeJson (Inject src target) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "inject")
      , Tuple "[]"(encodeJson src)
      , Tuple "=" (encodeJson target)
      ]
  encodeJson (Mult terms) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "mult")
      , Tuple "=" (encodeJson terms)
      ]
  encodeJson (Project term) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "field")
      , Tuple "=" (encodeJson term)
      ]


instance encodeJsonStage :: (EncodeJson (StrMap String), EncodeJson Map)
  => EncodeJson Stage where
  encodeJson (Map m) =
    encodeJson $ fromFoldable
      [ Tuple "@" (encodeJson "map")
      , Tuple "=" (encodeJson m)
      ]
  encodeJson (Reduce index m) =
    encodeJson $ fromFoldable $ concat
      [ [ Tuple "@" (encodeJson "reduce")
        , Tuple "=" (encodeJson m)
        ]
        , maybe [] (encodeJson >>> Tuple "#" >>> snoc []) index
      ]


instance encodeJsonJsonSchema :: EncodeJson JsonSchema where
  encodeJson (JObject schema) =
    encodeJson $ map encodeJson schema
  encodeJson (JArray schema) =
    encodeJson [encodeJson schema]
  encodeJson JNumber =
    encodeJson "number"
  encodeJson JString =
    encodeJson "string"
  encodeJson JBoolean =
    encodeJson "boolean"


-- INSTANCE SHOW
instance showTerminal :: EncodeJson Terminal => Show Terminal where
  show = encodeJson >>> stringify

instance showReduce :: EncodeJson Reduce => Show Reduce where
  show = encodeJson >>> stringify

instance showMap :: EncodeJson Map => Show Map where
  show = encodeJson >>> stringify

instance showStage :: EncodeJson Stage => Show Stage  where
  show = encodeJson >>> stringify

instance showJsonSchema :: EncodeJson JsonSchema => Show JsonSchema where
  show = encodeJson >>> stringify
