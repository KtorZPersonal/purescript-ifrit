const Ifrit = require("../dist")
const mongodb = require("mongodb")
const schema = require("./schema.json")

/*
 * Names and average size of the first three females order by height
 */
const query = `
    SELECT name, AVG(details.physical.height)
    WHERE details.physical.gender = "female"
    GROUP BY NULL
    ORDER BY details.physical.height
    LIMIT 3
`

mongodb.MongoClient
.connect("mongodb://localhost:27017/ifrit")
.then((db) => {
    return db
        .collection("mages")
        .aggregate(Ifrit.compile.mongodb(schema, query))
        .toArray()
})
.then((results) => {
    console.log(results)
    process.exit(0)
})
.catch((err) => {
    console.error(err)
    process.exit(1)
})
