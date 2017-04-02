const Ifrit = require("../dist")
const mongodb = require("mongodb")
const schema = require("./schema.json")

/*
 * Minimal age of female mages
 */
const query = `
    SELECT name, MIN(details.biographical.age) AS min_age
    WHERE details.physical.gender = "female"
    GROUP BY NULL
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
