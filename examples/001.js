const Ifrit = require("../dist")
const mongodb = require("mongodb")
const schema = require("./schema.json")

/*
 * Bad guys' names
 */
const query = `
    SELECT name
    WHERE bad_guy = true
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
