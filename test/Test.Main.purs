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

    test "[0] WHERE patate = NULL" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Where
          , L.Word "patate"
          , L.Binary L.Eq
          , L.Keyword L.Null
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "WHERE patate = NULL"
          })

    test "[0] SELECT patate ORDER BY autruche" do
      Assert.equal
        (Right $ fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Word "autruche"
          , L.EOF
          ])
        (evalStateT L.tokenize
          { pos: 0
          , str: "SELECT patate ORDER BY autruche"
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
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
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
            [ P.Projection $ P.Selector "patate" (Just "autruche")
            ])
          Nothing
          Nothing
          Nil
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
            [ P.Projection $ P.Selector "patate" Nothing
            , P.Projection $ P.Selector "autruche" Nothing
            ])
          Nothing
          Nothing
          Nil
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
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Gt (P.Field "autruche") (P.Number (fromInt 14)
          ))
          Nil
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
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          (Just $ P.Select
            (fromFoldable
              [ P.Projection $ P.Selector "autruche" Nothing
              ])
            Nothing
            Nothing
            Nil
          )
          Nothing
          Nil
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
        (Right $ P.Group
          P.IdxNull
          (fromFoldable
            [ P.Aggregation $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
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
            [ P.Projection $ P.Function L.Avg "patate" Nothing
            , P.Projection $ P.Function L.Count "things" (Just "c")
            ])
          Nothing
          (Just $ P.Term $ P.Factor $ P.Binary
            L.Neq (P.Field "autruche") (P.String "banana")
          )
          Nil
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
            [ P.Projection $ P.Selector "patate" Nothing
            ])
          Nothing
          Nothing
          Nil
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
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.Where
          , L.Word "patate"
          , L.Binary L.Eq
          , L.Keyword L.Null
          , L.EOF
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
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Word "autruche"
          , L.EOF
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
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Word "autruche"
          , L.Keyword L.Asc
          , L.EOF
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
        )
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Word "autruche"
          , L.Keyword L.Asc
          , L.Comma
          , L.Word "patate"
          , L.Keyword L.Desc
          , L.EOF
          ]))

    test "SELECT patate ORDER BY NULL" do
      Assert.equal
        (Left "parsing error: unexpected token: NULL" :: Either String P.Statement)
        (evalStateT P.parse (fromFoldable
          [ L.Keyword L.Select
          , L.Word "patate"
          , L.Keyword L.OrderBy
          , L.Keyword L.Null
          , L.EOF
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
                "$lt": ["$age", 16]
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
                  { "$lt": ["$age", 16] },
                  { "$eq": ["$class", "necromancer"] }
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


    test "SELECT power WHERE parent = NULL" do
      Assert.equal
        (jsonParser
          """
          [
            {
              "$match": {
                "$eq": ["$parent", null]
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

  --  ____                             _   _
  -- / ___|  ___ _ __ ___   __ _ _ __ | |_(_) ___
  -- \___ \ / _ \ '_ ` _ \ / _` | '_ \| __| |/ __|
  --  ___) |  __/ | | | | | (_| | | | | |_| | (__
  -- |____/ \___|_| |_| |_|\__,_|_| |_|\__|_|\___|
  --
  suite "semantic" do
    test "invalid index (aggregation) - unexpected field" do
      Assert.equal
        (Left "unexisting field: 'patate'")
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
            )
          )

    test "invalid condition (projection) - unexpected field" do
      Assert.equal
        (Left "unexisting field: 'patate'")
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
            )
          )

    test "invalid condition (projection) - type mismatch Lt" do
      Assert.equal
        (Left "invalid combination of types for '<' operator")
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
            )
          )

    test "invalid condition (projection) - type mismatch Lt (2)" do
      Assert.equal
        (Left "invalid combination of types for '<' operator")
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
            )
          )

    test "invalid condition (projection) - type mismatch Eq" do
      Assert.equal
        (Left "invalid combination of types for '=' operator")
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
            )
          )

    test "invalid condition (Aggrrgation) - unexpected field (2)" do
      Assert.equal
        (Left "unexisting field: 'patate'")
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
            )
          )


    test "invalid condition (aggregation) - type mismatch Neq" do
      Assert.equal
        (Left "invalid combination of types for '!=' operator")
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
            )
          )

    test "invalid selection - unexisting field" do
      Assert.equal
        (Left "unexisting field: 'patate'")
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
            )
          )

    test "invalid selection - group with alias as _id" do
      Assert.equal
        (Left "reserved field's name: _id")
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
            )
          )

    test "invalid selection - group with alias as _id (2)" do
      Assert.equal
        (Left "reserved field's name: _id")
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
            )
          )

    test "invalid function call - (projection) avg on non-array" do
      Assert.equal
        (Left "invalid operation: function 'AVG' applied to non array<number>")
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
            )
          )

    test "invalid order by - unexpected field" do
      Assert.equal
        (Left "unexisting field: 'patate'")
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
            )
          )
