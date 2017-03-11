module Ifrit.Decoder
  ( decode2
  , decode3
  , decode4
  ) where

import Prelude

import Data.Argonaut.Decode(class DecodeJson)
import Data.Argonaut.Decode.Combinators((.??))
import Data.Argonaut.Core(Json, toObject)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))


decode2 :: forall a1 a2 b. (DecodeJson a1, DecodeJson a2)
  => String -> String
  -> (Maybe a1 -> Maybe a2 -> Either String b)
  -> Json
  -> Either String b
decode2 k1 k2 decoder json =
  case toObject json of
    Just obj ->
      do arg1 <- obj .?? k1
         arg2 <- obj .?? k2
         decoder arg1 arg2
    Nothing ->
      Left "Invalid Operator"


decode3 :: forall a1 a2 a3 b. (DecodeJson a1, DecodeJson a2, DecodeJson a3)
  => String -> String -> String
  -> (Maybe a1 -> Maybe a2 -> Maybe a3 -> Either String b)
  -> Json
  -> Either String b
decode3 k1 k2 k3 decoder json =
  case toObject json of
    Just obj ->
      do arg1 <- obj .?? k1
         arg2 <- obj .?? k2
         arg3 <- obj .?? k3
         decoder arg1 arg2 arg3
    Nothing ->
      Left "Invalid Operator"


decode4 :: forall a1 a2 a3 a4 b. (DecodeJson a1, DecodeJson a2, DecodeJson a3, DecodeJson a4)
  => String -> String -> String -> String
  -> (Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Either String b)
  -> Json
  -> Either String b
decode4 k1 k2 k3 k4 decoder json =
  case toObject json of
    Just obj ->
      do arg1 <- obj .?? k1
         arg2 <- obj .?? k2
         arg3 <- obj .?? k3
         arg4 <- obj .?? k4
         decoder arg1 arg2 arg3 arg4
    Nothing ->
      Left "Invalid Operator"
