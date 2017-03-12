<p align="center">
    <img src="./.github/ifrit.png">
</p>

Ifrit ![travis](https://travis-ci.org/KtorZ/ifrit.svg?style=flat-square) ![license](https://img.shields.io/badge/License-MPL_2.0-blue.svg?style=flat-square)
=========

# Todo

- [x] ~~Add [schema](json-schema.org) validations to ensure type consistency~~
- [x] ~~Review test files and structure~~
- [x] ~~Add `reduce` stage~~
- [x] ~~Add `avg` reduce Operator~~
- [x] ~~Add `max` reduce Operator~~
- [x] ~~Add `min` reduce Operator~~
- [x] ~~Add `sum` reduce Operator~~
- [x] ~~Add `abs` map Operator~~
- [x] ~~Add `add` map Operator~~
- [x] ~~Add `ceil` map Operator~~
- [ ] Add `collect` map Operator
- [x] ~~Add `div` map Operator~~
- [x] ~~Add `floor` map Operator~~
- [x] ~~Add `mult` map Operator~~
- [x] ~~Add `gt`, `lt`, `eq`, `neq`, `and`, `or` filter Operators~~
- [ ] Add `every`, `some` filter Operators
- [x] ~~Add `?` filter option on the `map` stage~~
- [ ] Review tests to show all executed fixtures
- [ ] Write package documentation & extend README

# Examples 

## Schema Source 

```json
{
    "name": "string",
    "age": "number",
    "lvl": "number",
    "is_alive": "boolean",
    "spells": [{
        "name": "string",
        "power": "number"
    }]
}
```

## Sum of all level, age and a constant

```sql
SELECT SUM(age, lvl, 14)
```

```json
[
    {
        "@": "map",
        "=": {
            "sum": {
                "@": "add",
                "=": [
                    { "@": "constant", "=": 14 },
                    { "@": "field", "=": "lvl" },
                    { "@": "field", "=": "age" }
                ]
            }
        }
    }
]
```

## Maximum of spells' power

```sql
SELECT MAX(spells.power) AS max_power
```

```json
[
    {
        "@": "map",
        "=": {
            "max_power": {
                "@": "inject",
                "[]": { "@": "field", "=": "spells" },
                "=": {
                    "@": "max",
                    "=": { "@": "field", "=": "power" }
                }
            }
        }
    }
]
```

## Average spell power per class

```sql
SELECT AVG(power)
FROM (SELECT AVG(spells.power) as power, class)
GROUP BY class 
```

```json
[
    {
        "@": "map",
        "=": {
            "power": {
                "@": "inject",
                "[]": { "@": "field", "=": "spells" },
                "=": {
                    "@": "avg",
                    "=": { "@": "field", "=": "power" }
                }
            }
        }
    },
    {
        "@": "reduce",
        "#": { "@": "field", "=": "class" },
        "=": {
            "avg": {
                "@": "avg",
                "=": { "@": "field", "=": "power" }
            }
        }
    }
]
```

## Name of young mages

```sql
SELECT name
WHERE age < 16
```

```json
[
    {
        "@": "map",
        "?": {
            "@": "lt",
            "=": [
                { "@": "field", "=": "age" },
                { "@": "constant", "=": 14 }
            ]
        },
        "=": {
            "name": { "@": "field", "=": "name" }
        }
    }
]   
```

# Credits

- Chibi Ifrit by [capsicum](http://capsicum.deviantart.com/)
