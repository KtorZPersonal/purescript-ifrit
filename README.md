<p align="center">
    <img src="./.github/ifrit.png">
</p>

Ifrit ![travis](https://travis-ci.org/KtorZ/ifrit.svg?style=flat-square) ![license](https://img.shields.io/badge/License-MPL_2.0-blue.svg?style=flat-square)
=========

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

## Sum of all level grouped by age

```sql
SELECT SUM(lvl) GROUP BY age
```

```json
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
```

## Maximum of spells' power

```sql
SELECT MAX(spells.power) AS max_power
```

```json
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
```

## Average spell power per class

```sql
SELECT AVG(power)
FROM (SELECT class, AVG(spells.power) AS power)
GROUP BY class 
```

```json
[
    {
        "$project": {
            "class": "$class",
            "power": {
                "divide": [
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
```

## Name of young mages

```sql
SELECT name
WHERE age < 16
```

```json
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
```

# Credits

- Chibi Ifrit by [capsicum](http://capsicum.deviantart.com/)
