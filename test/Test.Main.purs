
module Test.Main where

import Prelude

import Control.Monad.State(evalStateT)
import Data.Decimal(fromNumber, fromInt)
import Data.Either(Either(..))
import Data.List(fromFoldable)
import Data.Maybe(Maybe(..))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Ifrit.Lexer as L
import Ifrit.Parser as P
-- import Ifrit.Driver.MongoDB(compile)

main = runTest do
  suite "lexer" do
    test "Nil" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: ""
          })
        (Right $ fromFoldable [])

    test "[0] SELECT patate" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: "SELECT patate"
          })
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          ])

    test "[0] SELECT (patate)" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: "SELECT (patate)"
          })
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          ])

    test "[0] patate     GROUP BY patate" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: "patate     GROUP BY patate"
          })
        (Right $ fromFoldable
          [ L.Word "patate"
          , L.Keyword  L.GroupBy
          , L.Word "patate"
          ])

    test "[2] - NULL patate AS alias" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 2
          , str: "- NULL patate AS alias"
          })
        (Right $ fromFoldable
          [ L.Keyword L.Null
          , L.Alias "patate" "alias"
          ])

    test "[0] WHERE ? = patate" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: "WHERE ? = patate"
          })
          (Left "invalid token at position 6")

    test "[0] FROM AVG(patate) > 14 OR .42 != 1.14" do
      Assert.equal
        (evalStateT L.tokenize
          { pos: 0
          , str: "FROM AVG(patate) > 14 OR .42 != 1.14"
          })
        (Right $ fromFoldable
          [ L.Keyword L.From
          , L.Function L.Avg
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.Binary L.Gt
          , L.Number (fromNumber 14.0)
          , L.Keyword L.Or
          , L.Number (fromNumber 0.42)
          , L.Binary L.Neq
          , L.Number (fromNumber 1.14)
          ])

  suite "parser" do
    test "SELECT patate" do
      Assert.equal
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          ]))
        (Right $ P.Select
          (P.Single "patate" Nothing)
          Nothing
          Nothing
          Nothing
        )

    test "SELECT patate AS autruche" do
      Assert.equal
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Alias "patate" "autruche"
          ]))
        (Right $ P.Select
          (P.Single "patate" (Just "autruche"))
          Nothing
          Nothing
          Nothing
        )

    test "SELECT (patate), autruche" do
      Assert.equal
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.Comma
          , L.Word "autruche"
          ]))
        (Right $ P.Select
          (P.Multiple $ fromFoldable
              [ P.Single "patate" Nothing
              , P.Single "autruche" Nothing
              ])
          Nothing
          Nothing
          Nothing
        )

    test "SELECT patate WHERE autruche > 14" do
      Assert.equal
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.Where
          , L.Word "autruche"
          , L.Binary L.Gt
          , L.Number (fromInt 14)
          ]))
        (Right $ P.Select
          (P.Single "patate" Nothing)
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
              L.Gt (P.Field "autruche") (P.Number (fromInt 14)
          ))
          Nothing
        )
