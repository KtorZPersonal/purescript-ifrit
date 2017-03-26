module Test.Main where

import Prelude

import Control.Monad.Aff.AVar(AVAR)
import Control.Monad.Eff(Eff)
import Control.Monad.Eff.Console(CONSOLE)
import Control.Monad.State(evalStateT)
import Data.Argonaut.Parser(jsonParser)
import Data.Decimal(fromNumber, fromInt)
import Data.Either(Either(..))
import Data.List(fromFoldable)
import Data.Maybe(Maybe(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console(TESTOUTPUT)
import Test.Unit.Main (runTest)

import Ifrit.Lexer as L
import Ifrit.Parser as P
import Ifrit.Driver.MongoDB(compile)


main :: Eff (avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT) Unit
main = runTest do
  --  _
  -- | |    _____  _____ _ __
  -- | |   / _ \ \/ / _ \ '__|
  -- | |__|  __/>  <  __/ |
  -- |_____\___/_/\_\___|_|
  --
  suite "lexer" do
    test "Nil" do
      Assert.equal
        (Right $ fromFoldable [L.EOF])
        (evalStateT L.tokenize
          { pos: 0
          , str: ""
          })

    test "[0] SELECT patate" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "SELECT patate"
          })

    test "[0] SELECT (patate)" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "SELECT (patate)"
          })

    test "[0] patate     GROUP BY patate" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Word "patate"
          , L.Keyword  L.GroupBy
          , L.Word "patate"
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "patate     GROUP BY patate"
          })

    test "[2] - NULL patate AS alias" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Null
          , L.Word "patate"
          , L.Keyword L.As
          , L.Word "alias"
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 2
          , str: "- NULL patate AS alias"
          })

    test "[0] WHERE ? = patate" do
      Assert.equal
        (Left "invalid token '?' at position 6")
        (evalStateT L.tokenize
          { pos: 0
          , str: "WHERE ? = patate"
          })

    test "[0] FROM AVG(patate) > 14 OR .42 != 1.14" do
      Assert.equal
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
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "FROM AVG(patate) > 14 OR .42 != 1.14"
          })


  --  ____
  -- |  _ \ __ _ _ __ ___  ___ _ __
  -- | |_) / _` | '__/ __|/ _ \ '__|
  -- |  __/ (_| | |  \__ \  __/ |
  -- |_|   \__,_|_|  |___/\___|_|
  --
  suite "parser" do
    test "SELECT patate" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.EOF
          ]))

    test "SELECT patate AS autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" (Just "autruche")
            ])
          Nothing
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.As
          , L.Word "autruche"
          , L.EOF
          ]))

    test "SELECT (patate), autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            , P.Selector "autruche" Nothing
            ])
          Nothing
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.Comma
          , L.Word "autruche"
          , L.EOF
          ]))

    test "SELECT patate WHERE autruche > 14" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Gt (P.Field "autruche") (P.Number (fromInt 14)
          ))
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.Where
          , L.Word "autruche"
          , L.Binary L.Gt
          , L.Number (fromInt 14)
          , L.EOF
          ]))

    test "SELECT patate FROM (SELECT autruche)" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          (Just $ P.Select
            (fromFoldable
              [ P.Selector "autruche" Nothing
              ])
            Nothing
            Nothing
            Nothing
          )
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.From
          , L.Parenthesis L.Open
          , L.Keyword L.Select
          , L.Word "autruche"
          , L.Parenthesis L.Close
          , L.EOF
          ]))

    test "SELECT patate GROUP BY NULL" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          (Just $ P.IdxNull)
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.GroupBy
          , L.Keyword L.Null
          , L.EOF
          ]))

    test "SELECT patate GROUP BY NULL (no EOF)" do
      Assert.equal
        (Left "parsing error: invalid end of input" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.GroupBy
          , L.Keyword L.Null
          ]))

    test "SELECT patate GROUP BY NULL WHERE 14.0 > 42.0" do
      Assert.equal
        (Left "parsing error: invalid end of input" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.GroupBy
          , L.Keyword L.Null
          , L.Keyword L.Where
          , L.Number (fromNumber 14.0)
          , L.Binary L.Gt
          , L.Number (fromNumber 42.0)
          , L.EOF
          ]))

    test "SELECT patate WHERE 14.0 = 42.0 GROUP BY autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Eq (P.Number (fromNumber 14.0)) (P.Number (fromNumber 42.0))
          )
          (Just $ P.IdxField "autruche")
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.Where
          , L.Number (fromNumber 14.0)
          , L.Binary L.Eq
          , L.Number (fromNumber 42.0)
          , L.Keyword L.GroupBy
          , L.Word "autruche"
          , L.EOF
          ]))

    test "SELECT AVG(patate), COUNT(things) AS c WHERE autruche != \"banana\"" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Function L.Avg "patate" Nothing
            , P.Function L.Count "things" (Just "c")
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Neq (P.Field "autruche") (P.String "banana")
          )
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Function L.Avg
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.Comma
          , L.Function L.Count
          , L.Parenthesis L.Open
          , L.Word "things"
          , L.Parenthesis L.Close
          , L.Keyword L.As
          , L.Word "c"
          , L.Keyword L.Where
          , L.Word "autruche"
          , L.Binary L.Neq
          , L.String "banana"
          , L.EOF
          ]))

    test "SELECT (patate, autruche" do
      Assert.equal
        (Left "parsing error: unbalanced parenthesis expression" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Comma
          , L.Word "autruche"
          , L.EOF
          ]))

    test "SELECT AVG(14)" do
      Assert.equal
        (Left "parsing error: unexpected token: 14" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Function L.Avg
          , L.Parenthesis L.Open
          , L.Number $ fromInt 14
          , L.Parenthesis L.Close
          , L.EOF
          ]))

    test "SELECT AVG(patate, autruche)" do
      Assert.equal
        (Left "parsing error: invalid argument(s) for function AVG" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Function L.Avg
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Comma
          , L.Word "autruche"
          , L.Parenthesis L.Close
          , L.EOF
          ]))

    test "SELECT (((((patate)))))" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Parenthesis L.Open
          , L.Parenthesis L.Open
          , L.Parenthesis L.Open
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.Parenthesis L.Close
          , L.Parenthesis L.Close
          , L.Parenthesis L.Close
          , L.Parenthesis L.Close
          , L.EOF
          ]))

  suite "driver - MongoDB" do
    test "SELECT SUM(lvl) GROUP BY age" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$group": {
                "_id": "$age",
                "lvl": {
                  "$sum": "$lvl"
                }
              }
            }
          ]
          """)
        (compile "SELECT SUM(lvl) GROUP BY age")

    test "SELECT MAX(spells.power) AS max_power" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$project": {
                "max_power": {
                  "$reduce": {
                    "input": "$spells",
                    "in": {
                      "$cond": {
                        "if": {
                          "$or": [
                            { "$eq": ["$$value", null] },
                            { "$gt": ["$power", "$$value"] }
                          ]
                        },
                        "then": "$power",
                        "else": "$$value"
                      }
                    },
                    "initialValue": null
                  }
                }
              }
            }
          ]
          """)
          (compile "SELECT MAX(spells.power) AS max_power")

