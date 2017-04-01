![](.github/ifrit.png)

Ifrit ![](https://travis-ci.org/KtorZ/purescript-ifrit.svg?style=flat-square) ![](https://img.shields.io/badge/license-MPL_2.0-blue.svg?style=flat-square) ![](https://img.shields.io/badge/doc-javascript-red.svg?style=flat-square) ![](https://img.shields.io/badge/doc-purescript-c5c5c5.svg?style=flat-square) 
=========

# Getting started

## Installation

```
npm install ifrit
```

## Usage

```js
const Ifrit = require("ifrit")
const mongodb = require("mongodb")

// Define a schema for the collection
const schema = {
    "amount": "number",
    "account": {
        "country": "string",
        "currency": "string"
    },
    "items": [{
        "price": "number",
        "description": "string"
    }],
    "created_at": "datetime"
}

// Define / Retrieve an aggregation query
const query = `SELECT AVG(items.price) WHERE account.country = "NL" AND amount > 250`

// Aggregate results from Mongodb
mongodb.MongoClient
    .connect("mongodb://localhost:27017")
    .then((db) => {
        return db
            .collection("transactions")
            .aggregate(Ifrit.mongodb.compile(schema, query))
    })
    .then(console.log)
    .catch(console.err)
```


## Documentation

Ifrit is available as a Node.js module as well as a PureScript module. The JavaScript
documentation is accessible on [github pages](https://ktorz.github.io/purescript-ifrit). A
documentation for PureScript is published on
[pursuit](https://pursuit.purescript.org/packages/purescript-ifrit/).

## Examples 

Different scenarios are available on the repository in the `examples` folder. Here's a
summary of all examples:

- TODO


# Overview

## What & Why ?

Ifrit is a **compiler from SQL to NoSQL** for data aggregation. NoSQL databases are great at
modelling structured, unstructured and polymorphic data and usually offer powerful map-reduce
based API when it comes to data aggregation. However, it is truly challenging to leverage those
API in a web service. 

In a nutshell, Ifrit:

- Offers **aggegation capabilities** to any API via a neat and well-known syntax
- Ensures the **semantic correctness** of a request
- **Embraces security** concerns by clearly defining the scope of each request
- Produces an **easy-to-use output**, without any dependencies or pre-requisite setup
- Has a **small fingerprint** on your integration and performances


## How it works 

Ifrit builds an Abstract Syntax Tree (AST) which represents only syntactycally correct
requests. Then, it generates a request corresponding to a specific driver. So far, MongoDB is
the only target driver available.

```
       _                                                                                    __
        \                                                                                  /
         \   +----------+         +-----------+        +----------+        +----------+   /
    SQL   = =| tokenize +--------->   parse   +-------->  verify  +--------> generate |= =   NoSQL
         /   +----------+         +-----------+        +----------+        +----------+   \
       _/                                                                                  \__
                                                                                       
```

#### Example (MongoDB)

**input**
```sql
SELECT COUNT(_id) AS nb_txs WHERE amount > 1000 GROUP BY account.currency
```

**output**
```json
[
    {
        "$match": {
            "$gt": [
                "$amount",
                1000
            ]
        }
    },
    {
        "$group": {
            "_id": "$account.currency",
            "nb_txs": {
                "$sum": 1
            }
        }
    }
]
```

## Schema definition

Ifrit acts on a single collection at a time and does not support joins. Therefore, a schema is
required in order to verify the request (semantically and security wise). Schemas are defined
as JSON objects in a declarative syntaxe.

Ifrit supports the following primitive types: `number`, `string`, `boolean`, `datetime`. Arrays
and objects can be declared by nesting primitive types in JSON arrays `[]` or objects `{}`. 

> Ifrit can only see what's defined in a schema. The compilation will fail if the request tries
> to use or select a field not present in the schema. This can be used to control the scope of
> what elements are accessible via the query.

#### Example:

```json
{
    "amount": "number",
    "account": {
        "country": "string",
        "currency": "string"
    },
    "items": [{
        "price": "number",
        "description": "string"
    }],
    "created_at": "datetime"
}
```


## SQL language support

type        | support
--------    | --------
projection  | `SELECT, AS, FROM`
grouping    | `GROUP BY, DISTINCT`
filtering   | `WHERE, LIMIT, OFFSET`
sorting     | `ORDER BY, DESC, ASC`
operators   | `AND, OR, NOT, =, !=, >, <`

function | applicable type
-------- | ----------------
AVG      | number
COUNT    | any
MAX      | number
MIN      | number
SUM      | number

> ⚠ Ifrit relies on a strict order of clauses ⚠
>  
> - (1) `SELECT`
> - (2) `DISTINCT`
> - (3) `FROM`
> - (4) `WHERE`
> - (5) `GROUP BY`
> - (6) `ORDER BY`
> - (7) `(ASC | DESC)`
> - (8) `LIMIT`
> - (9) `OFFSET`

#### Differences with SQL

- Ifrit is case-sensitive, e.g. `AS != as`, `NULL != null`, etc.

- Ifrit doesn't support the `*` selector

- `ORDER BY` can't be use with `NULL`

- Ifrit can't `JOIN` from other collections, the `FROM` can only be used to defined derived
  tables, i.e, define a multi-level pipeline of map / reduce operations.

- Aggregation functions can be applied to numbers when used with `GROUP BY` or directly to
  array of numbers when apply without. Ifrit also supports nested notation for array of objects
  (e.g.  `SELECT AVG(items.price)`).

- When no aliase is specified, the argument is used to name the property in the output schema. 



# Benchmark

On a classic i7 quad-core, 16Gb DDR5 RAM / Ubuntu 16.04

```
SELECT age 
> 9,795 ops/sec ±0.40% (91 runs sampled)

SELECT class AS klass, COUNT(bonus)
> 4,791 ops/sec ±0.83% (90 runs sampled)

SELECT AVG(age) GROUP BY class
> 5,754 ops/sec ±0.58% (94 runs sampled)

SELECT is_master WHERE age > 14 AND age < 20
> 4,586 ops/sec ±0.65% (93 runs sampled)

SELECT AVG(power) AS avg_pow FROM (SELECT AVG(spells.power), age) WHERE age > 18 GROUP BY NULL 
> 2,378 ops/sec ±0.63% (93 runs sampled)
```

# Changelog 

### yyy-mm-dd | 1.0 

- TODO

# Compatibility

- MongoDB: ~3.4

# Credits

- Chibi Ifrit by [capsicum](http://capsicum.deviantart.com/)
