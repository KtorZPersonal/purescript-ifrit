module Test.Main where

import Prelude

import Control.Monad.Aff.AVar(AVAR)
import Control.Monad.Eff(Eff)
import Control.Monad.Eff.Console(CONSOLE)
import Control.Monad.State(evalStateT)
import Data.Argonaut.Core(Json)
import Data.Argonaut.Parser(jsonParser)
import Data.Decimal(fromNumber, fromInt)
import Data.Either(Either(..))
import Data.List(List(..), fromFoldable, (:))
import Data.Maybe(Maybe(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console(TESTOUTPUT)
import Test.Unit.Main (runTest)

import Ifrit.Driver.MongoDB as M
import Ifrit.Lexer as L
import Ifrit.Parser as P
import Ifrit.Semantic as S


ingest :: String -> Either String Json
ingest query = do
  tokens <- evalStateT L.tokenize { pos: 0, str: query }
  ast :: P.Statement <- evalStateT P.parse tokens
  M.ingest ast

tokenize :: Int -> String  -> Either String (List L.Token)
tokenize pos str = do
  xs <- (evalStateT L.tokenize { pos, str })
  pure $ map (\{ token } -> token) xs




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
        (Right $ fromFoldable
          [ L.EOF
          ])
        (tokenize 0 "")

    test "[0] SELECT patate" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.EOF
          ])
          (tokenize 0 "SELECT patate")

    test "[0] SELECT (patate)" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.EOF
          ])
        (tokenize 0 "SELECT (patate)")

    test "[0] patate     GROUP BY patate" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Word "patate"
          , L.Keyword  L.GroupBy
          , L.Word "patate"
          , L.EOF
          ])
        (tokenize 0 "patate     GROUP BY patate")

    test "[2] - NULL patate AS alias" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Null
          , L.Word "patate"
          , L.Keyword L.As
          , L.Word "alias"
          , L.EOF
          ])
          (tokenize 2 "- NULL patate AS alias")

    test "[0] WHERE ? = patate" do
      Assert.equal
        (Left "invalid token '?' at position 6")
        (tokenize 0 "WHERE ? = patate")

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
          (tokenize 0 "FROM AVG(patate) > 14 OR .42 != 1.14")

    test "[0] WHERE patate = NULL" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Where
          , L.Word "patate"
          , L.Binary L.Eq
          , L.Keyword L.Null
          , L.EOF
          ])
          (tokenize 0 "WHERE patate = NULL")

    test "[0] SELECT patate ORDER BY autruche" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Word "autruche"
          , L.EOF
          ])
          (tokenize 0 "SELECT patate ORDER BY autruche")

    test "[0] patate LIMIT 14" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Word "patate"
          , L.Keyword L.Limit
          , L.Number (fromInt 14)
          , L.EOF
          ])
          (tokenize 0 "patate LIMIT 14")

    test "[0] patate OFFSET 1.4" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Word "patate"
          , L.Keyword L.Offset
          , L.Number (fromNumber 1.4)
          , L.EOF
          ])
          (tokenize 0 "patate OFFSET 1.4")

    test "[0] NOT(patate)" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Unary L.Not
          , L.Parenthesis L.Open
          , L.Word "patate"
          , L.Parenthesis L.Close
          , L.EOF
          ])
          (tokenize 0 "NOT (patate)")

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
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.EOF, pos: 2 }
          ]))

    test "SELECT patate AS autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" (Just "autruche")
            ])
          Nothing
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.As, pos: 2 }
          , { token: L.Word "autruche", pos: 3 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT (patate), autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            , P.Projection $ P.Selector "autruche" Nothing
            ])
          Nothing
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Parenthesis L.Open, pos: 1 }
          , { token: L.Word "patate", pos: 2 }
          , { token: L.Parenthesis L.Close, pos: 3 }
          , { token: L.Comma, pos: 4 }
          , { token: L.Word "autruche", pos: 5 }
          , { token: L.EOF, pos: 6 }
          ]))

    test "SELECT patate WHERE autruche > 14" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Gt (P.Field "autruche") (P.Number (fromInt 14)
          ))
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Where, pos: 2 }
          , { token: L.Word "autruche", pos: 3 }
          , { token: L.Binary L.Gt, pos: 4 }
          , { token: L.Number (fromInt 14), pos: 5 }
          , { token: L.EOF, pos: 6 }
          ]))

    test "SELECT patate FROM (SELECT autruche)" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          (Just $ P.Select
            (fromFoldable
              [ P.Projection $ P.Selector "autruche" Nothing
              ])
            Nothing
            Nothing
            Nil
            Nothing
            Nothing
          )
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.From, pos: 2 }
          , { token: L.Parenthesis L.Open, pos: 3 }
          , { token: L.Keyword L.Select, pos: 4 }
          , { token: L.Word "autruche", pos: 5 }
          , { token: L.Parenthesis L.Close, pos: 6 }
          , { token: L.EOF, pos: 7 }
          ]))

    test "SELECT patate GROUP BY NULL" do
      Assert.equal
        (Right $ P.Group
          P.IdxNull
          (fromFoldable
            [ P.Aggregation $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.GroupBy, pos: 2 }
          , { token: L.Keyword L.Null, pos: 3 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT patate GROUP BY NULL (no EOF)" do
      Assert.equal
        (Left "unexpected end of input" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.GroupBy, pos: 2 }
          , { token: L.Keyword L.Null, pos: 3 }
          ]))

    test "SELECT patate GROUP BY NULL WHERE 14.0 > 42.0" do
      Assert.equal
        (Left "unexpected end of input" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.GroupBy, pos: 2 }
          , { token: L.Keyword L.Null, pos: 3 }
          , { token: L.Keyword L.Where, pos: 4 }
          , { token: L.Number (fromNumber 14.0), pos: 5 }
          , { token: L.Binary L.Gt, pos: 6 }
          , { token: L.Number (fromNumber 42.0), pos: 7 }
          , { token: L.EOF, pos: 8 }
          ]))

    test "SELECT patate WHERE 14.0 = 42.0 GROUP BY autruche" do
      Assert.equal
        (Right $ P.Group
          (P.IdxField "autruche")
          (fromFoldable
            [ P.Aggregation $ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Eq (P.Number (fromNumber 14.0)) (P.Number (fromNumber 42.0))
          )
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Where, pos: 2 }
          , { token: L.Number (fromNumber 14.0), pos: 3 }
          , { token: L.Binary L.Eq, pos: 4 }
          , { token: L.Number (fromNumber 42.0), pos: 5 }
          , { token: L.Keyword L.GroupBy, pos: 6 }
          , { token: L.Word "autruche", pos: 7 }
          , { token: L.EOF, pos: 8 }
          ]))

    test "SELECT AVG(patate), COUNT(things) AS c WHERE autruche != \"banana\"" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Function L.Avg "patate" Nothing
            , P.Projection $ P.Function L.Count "things" (Just "c")
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Neq (P.Field "autruche") (P.String "banana")
          )
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Function L.Avg, pos: 1 }
          , { token: L.Parenthesis L.Open, pos: 2 }
          , { token: L.Word "patate", pos: 3 }
          , { token: L.Parenthesis L.Close, pos: 4 }
          , { token: L.Comma, pos: 5 }
          , { token: L.Function L.Count, pos: 6 }
          , { token: L.Parenthesis L.Open, pos: 7 }
          , { token: L.Word "things", pos: 8 }
          , { token: L.Parenthesis L.Close, pos: 9 }
          , { token: L.Keyword L.As, pos: 10 }
          , { token: L.Word "c", pos: 11 }
          , { token: L.Keyword L.Where, pos: 12 }
          , { token: L.Word "autruche", pos: 13 }
          , { token: L.Binary L.Neq, pos: 14 }
          , { token: L.String "banana", pos: 15 }
          , { token: L.EOF, pos: 16 }
          ]))

    test "SELECT (patate, autruche" do
      Assert.equal
        (Left "unbalanced parenthesis expression: expected `)` but got: EOF at position 5" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Parenthesis L.Open, pos: 1 }
          , { token: L.Word "patate", pos: 2 }
          , { token: L.Comma, pos: 3 }
          , { token: L.Word "autruche", pos: 4 }
          , { token: L.EOF, pos: 5 }
          ]))

    test "SELECT AVG(14)" do
      Assert.equal
        (Left "unexpected token: 14 at position 3" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Function L.Avg, pos: 1 }
          , { token: L.Parenthesis L.Open, pos: 2 }
          , { token: L.Number $ fromInt 14, pos: 3 }
          , { token: L.Parenthesis L.Close, pos: 4 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT AVG(patate, autruche)" do
      Assert.equal
        (Left "AVG has an invalid argument at position 2" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Function L.Avg, pos: 1 }
          , { token: L.Parenthesis L.Open, pos: 2 }
          , { token: L.Word "patate", pos: 3 }
          , { token: L.Comma, pos: 4 }
          , { token: L.Word "autruche", pos: 5 }
          , { token: L.Parenthesis L.Close, pos: 6 }
          , { token: L.EOF, pos: 7 }
          ]))

    test "SELECT (((((patate)))))" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Parenthesis L.Open, pos: 1 }
          , { token: L.Parenthesis L.Open, pos: 2 }
          , { token: L.Parenthesis L.Open, pos: 3 }
          , { token: L.Parenthesis L.Open, pos: 4 }
          , { token: L.Parenthesis L.Open, pos: 5 }
          , { token: L.Word "patate", pos: 6 }
          , { token: L.Parenthesis L.Close, pos: 7 }
          , { token: L.Parenthesis L.Close, pos: 8 }
          , { token: L.Parenthesis L.Close, pos: 9 }
          , { token: L.Parenthesis L.Close, pos: 10 }
          , { token: L.Parenthesis L.Close, pos: 11 }
          , { token: L.EOF, pos: 12 }
          ]))

    test "SELECT patate WHERE patate = NULL" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Eq (P.Field "patate") P.Null
          )
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Where, pos: 2 }
          , { token: L.Word "patate", pos: 3 }
          , { token: L.Binary L.Eq, pos: 4 }
          , { token: L.Keyword L.Null, pos: 5 }
          , { token: L.EOF, pos: 6 }
          ]))

    test "SELECT patate ORDER BY autruche" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          (P.OrderAsc "autruche" : Nil)
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.OrderBy, pos: 2 }
          , { token: L.Word "autruche", pos: 3 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT patate ORDER BY autruche ASC" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          (P.OrderAsc "autruche" : Nil)
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.OrderBy, pos: 2 }
          , { token: L.Word "autruche", pos: 3 }
          , { token: L.Keyword L.Asc, pos: 4 }
          , { token: L.EOF, pos: 5 }
          ]))

    test "SELECT patate ORDER BY autruche ASC, patate DESC" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          (P.OrderAsc "autruche" : P.OrderDesc "patate" : Nil)
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.OrderBy, pos: 2 }
          , { token: L.Word "autruche", pos: 3 }
          , { token: L.Keyword L.Asc, pos: 4 }
          , { token: L.Comma, pos: 5 }
          , { token: L.Word "patate", pos: 6 }
          , { token: L.Keyword L.Desc, pos: 7 }
          , { token: L.EOF, pos: 8 }
          ]))

    test "SELECT patate ORDER BY NULL" do
      Assert.equal
        (Left "unexpected token: NULL at position 3" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.OrderBy, pos: 2 }
          , { token: L.Keyword L.Null, pos: 3 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT patate LIMIT 14 OFFSET 42" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
          (Just $ P.Limit 14)
          (Just $ P.Offset 42)
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Limit, pos: 2 }
          , { token: L.Number (fromInt 14), pos: 3 }
          , { token: L.Keyword L.Offset, pos: 4 }
          , { token: L.Number (fromInt 42), pos: 5 }
          , { token: L.EOF, pos: 6 }
          ]))

    test "SELECT patate LIMIT 14.42" do
      Assert.equal
        (Left "LIMIT must be an integer at position 3" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Limit, pos: 2 }
          , { token: L.Number (fromNumber 14.42), pos: 3 }
          , { token: L.EOF, pos: 4 }
          ]))

    test "SELECT patate WHERE NOT age > 12" do
      Assert.equal
        (Right $ P.Select
          (fromFoldable
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Unary
            L.Not (P.Term $ P.Factor $ P.Binary
              L.Gt (P.Field "age") (P.Number $ fromInt 12)
            )
          )
          Nil
          Nothing
          Nothing
        )
        (evalStateT P.parse (fromFoldable
          [ { token: L.Keyword L.Select, pos: 0 }
          , { token: L.Word "patate", pos: 1 }
          , { token: L.Keyword L.Where, pos: 2 }
          , { token: L.Unary L.Not, pos: 3 }
          , { token: L.Word "age", pos: 4 }
          , { token: L.Binary L.Gt, pos: 5 }
          , { token: L.Number $ fromInt 12, pos: 6 }
          , { token: L.EOF, pos: 7 }
          ]))

  --  ____       _
  -- |  _ \ _ __(_)_   _____ _ __
  -- | | | | '__| \ \ / / _ \ '__|
  -- | |_| | |  | |\ V /  __/ |
  -- |____/|_|  |_| \_/ \___|_|
  --
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
        (ingest "SELECT SUM(lvl) GROUP BY age")

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
          (ingest "SELECT MAX(spells.power) AS max_power")

    test "SELECT AVG(power) FROM (SELECT class, AVG(spells.power) AS power) GROUP BY class" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$project": {
                "class": "$class",
                "power": {
                  "$divide": [
                    {
                      "$reduce": {
                        "input": "$spells",
                        "initialValue": 0,
                        "in": {
                          "$add": ["$$value", "$power"]
                        }
                      }
                    },
                    {
                      "$size": "$spells"
                    }
                  ]
                }
              }
            },
            {
              "$group": {
                "_id": "$class",
                "power": {
                  "$avg": "$power"
                }
              }
            }
          ]
          """)
          (ingest "SELECT AVG(power) FROM (SELECT class, AVG(spells.power) AS power) GROUP BY class")

    test "SELECT name WHERE age < 16" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "age": {
                  "$lt": 16
                }
              }
            },
            {
              "$project": {
                "name": "$name"
              }
            }
          ]
          """)
          (ingest "SELECT name WHERE age < 16")

    test "SELECT AVG(power) AS pwr WHERE age < 16 AND class = \"necromancer\"" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "$and": [
                  { "age": { "$lt": 16 } },
                  { "class": { "$eq": "necromancer" } }
                ]
              }
            },
            {
              "$project": {
                "pwr": {
                  "$avg": "$power"
                }
              }
            }
          ]
          """)
          (ingest "SELECT AVG(power) AS pwr WHERE age < 16 AND class = \"necromancer\"")

    test "SELECT AVG(power) AS pwr WHERE (age < 16 OR class = \"priest\") AND class = \"necromancer\"" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "$and": [
                  {
                    "$or": [
                      { "age": { "$lt": 16 } },
                      { "class": { "$eq": "priest" } }
                    ]
                  },
                  {
                    "class": { "$eq": "necromancer" }
                  }
                ]
              }
            },
            {
              "$project": {
                "pwr": {
                  "$avg": "$power"
                }
              }
            }
          ]
          """)
          (ingest "SELECT AVG(power) AS pwr WHERE (age < 16 OR class = \"priest\") AND class = \"necromancer\"")

    test "SELECT power WHERE parent = NULL" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "parent": { "$eq": null }
              }
            },
            {
              "$project": {
                "power": "$power"
              }
            }
          ]
          """)
          (ingest "SELECT power WHERE parent = NULL")

    test "SELECT class WHERE NOT(is_necromancer)" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "is_necromancer": false
              }
            },
            {
              "$project": {
                "class": "$class"
              }
            }
          ]
          """)
          (ingest "SELECT class WHERE NOT(is_necromancer)")

    test "SELECT power ORDER BY name" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$sort": {
                "name": 1
              }
            },
            {
              "$project": {
                "power": "$power"
              }
            }
          ]
          """)
          (ingest "SELECT power ORDER BY name")

    test "SELECT power ORDER BY name DESC, age" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$sort": {
                "name": -1,
                "age": 1
              }
            },
            {
              "$project": {
                "power": "$power"
              }
            }
          ]
          """)
          (ingest "SELECT power ORDER BY name DESC, age")

    test "SELECT MAX(power) GROUP BY NULL ORDER BY age ASC" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$sort": {
                "age": 1
              }
            },
            {
              "$group": {
                "_id": null,
                "power": {
                  "$max": "$power"
                }
              }
            }
          ]
          """)
          (ingest "SELECT MAX(power) GROUP BY NULL ORDER BY age ASC")

    test "SELECT power LIMIT 14 OFFSET 42" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$limit": 14
            },
            {
              "$skip": 42
            },
            {
              "$project": {
                "power": "$power"
              }
            }
          ]
          """)
          (ingest "SELECT power LIMIT 14 OFFSET 42")

    test "SELECT MAX(power) GROUP BY NULL ORDER BY age LIMIT 14" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$sort": {
                "age": 1
              }
            },
            {
              "$limit": 14
            },
            {
              "$group": {
                "_id": null,
                "power": {
                  "$max": "$power"
                }
              }
            }
          ]
          """)
          (ingest "SELECT MAX(power) GROUP BY NULL ORDER BY age LIMIT 14")

    test "SELECT patate WHERE NOT(patate > 12 AND patate = 14)" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "$or": [
                  { "patate": { "$lte": 12 } },
                  { "patate": { "$neq": 14 } }
                ]
              }
            },
            {
              "$project": {
                "patate": "$patate"
              }
            }
          ]
          """)
          (ingest "SELECT patate WHERE NOT(patate > 12 AND patate = 14)")


  --  ____                             _   _
  -- / ___|  ___ _ __ ___   __ _ _ __ | |_(_) ___
  -- \___ \ / _ \ '_ ` _ \ / _` | '_ \| __| |/ __|
  --  ___) |  __/ | | | | | (_| | | | | |_| | (__
  -- |____/ \___|_| |_| |_|\__,_|_| |_|\__|_|\___|
  --
  suite "semantic" do
    test "invalid index (aggregation) - unexpected field" do
      Assert.equal
        (Left "unexisting field 'patate' in GROUP BY expression")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "number"
            }
            """
          S.analyze schema (P.Group
              (P.IdxField "patate")
              (fromFoldable
                [ P.Aggregation $ P.Selector "autruche" Nothing
                ])
              Nothing
              Nothing
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid condition (projection) - unexpected field" do
      Assert.equal
        (Left "unexisting field 'patate' in WHERE expression")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "number"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Eq (P.Field "patate") (P.String "banana")
              )
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid condition (projection) - type mismatch Lt" do
      Assert.equal
        (Left "incompatible types \"number\", \"string\" with binary operator <")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "number"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Lt (P.Field "autruche") (P.String "banana")
              )
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid condition (projection) - type mismatch Lt (2)" do
      Assert.equal
        (Left "incompatible types \"string\", \"number\" with binary operator <")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Lt (P.Field "autruche") (P.Number $ fromInt 14)
              )
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid condition (projection) - type mismatch Eq" do
      Assert.equal
        (Left "incompatible types \"string\", \"number\" with binary operator =")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Eq (P.Field "autruche") (P.Number $ fromInt 14)
              )
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid condition (Aggrrgation) - unexpected field (2)" do
      Assert.equal
        (Left "unexisting field 'patate' in WHERE expression")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "boolean"
            }
            """
          S.analyze schema (P.Group
              P.IdxNull
              (fromFoldable
                [ P.Aggregation $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Neq (P.Field "patate") (P.Boolean true)
              )
              Nil
              Nothing
              Nothing
            )
          )


    test "invalid condition (aggregation) - type mismatch Neq" do
      Assert.equal
        (Left "incompatible types \"string\", \"number\" with binary operator !=")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Group
              (P.IdxField "autruche")
              (fromFoldable
                [ P.Aggregation $ P.Selector "autruche" Nothing
                ])
              Nothing
              (Just $ P.Term $ P.Factor $ P.Binary
                L.Neq (P.Field "autruche") (P.Number $ fromInt 14)
              )
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid selection - unexisting field" do
      Assert.equal
        (Left "unexisting field 'patate' in SELECT expression")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Group
              (P.IdxField "autruche")
              (fromFoldable
                [ P.Aggregation $ P.Selector "patate" Nothing
                ])
              Nothing
              Nothing
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid selection - group with alias as _id" do
      Assert.equal
        (Left "reserved field's name '_id'")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Group
              P.IdxNull
              (fromFoldable
                [ P.Aggregation $ P.Selector "autruche" (Just "_id")
                ])
              Nothing
              Nothing
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid selection - group with alias as _id (2)" do
      Assert.equal
        (Left "reserved field's name '_id'")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Group
              P.IdxNull
              (fromFoldable
                [ P.Aggregation $ P.Selector "_id" Nothing
                ])
              Nothing
              Nothing
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid function call - (projection) avg on non-array" do
      Assert.equal
        (Left "incompatible type \"string\" with function AVG")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "string"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Function L.Avg "autruche" Nothing
                ])
              Nothing
              Nothing
              Nil
              Nothing
              Nothing
            )
          )

    test "invalid order by - unexpected field" do
      Assert.equal
        (Left "unexisting field 'patate' in ORDER BY expression")
        (do
          schema <- S.fromString
            """
            {
              "autruche": "number"
            }
            """
          S.analyze schema (P.Select
              (fromFoldable
                [ P.Projection $ P.Selector "autruche" Nothing
                ])
              Nothing
              Nothing
              (P.OrderAsc "patate" : Nil)
              Nothing
              Nothing
            )
          )

    test "SELECT COUNT(name) GROUP BY age" do
      Assert.equal
        (S.fromString
          """
          {
            "name": "number",
            "_id": "number"
          }
          """)
          (do
            schema <- S.fromString
              """
              {
                "age": "number",
                "name": "string"
              }
              """
            S.analyze schema (P.Group
                (P.IdxField "age")
                (fromFoldable
                  [ P.Aggregation $ P.Function L.Count "name" Nothing
                  ])
                Nothing
                Nothing
                Nil
                Nothing
                Nothing
              )
            )

    test "SELECT COUNT(spells)" do
      Assert.equal
        (S.fromString
          """
          {
            "spells": "number"
          }
          """)
          (do
            schema <- S.fromString
              """
              {
                "spells": [{
                  "name": "string",
                  "power": "number"
                }]
              }
              """
            S.analyze schema (P.Select
                (fromFoldable
                  [ P.Projection $ P.Function L.Count "spells" Nothing
                  ])
                Nothing
                Nothing
                Nil
                Nothing
                Nothing
              )
            )
