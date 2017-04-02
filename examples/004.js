const Ifrit = require("../dist")
const mongodb = require("mongodb")
const schema = require("./schema.json")

/*
 * Names and classes of the first two good guys, ordered by age
 */
const query = `
    SELECT name, details.biographical.class AS class, details.biographical.age AS age
    WHERE NOT(bad_guy) AND details.physical.gender = "male"
    ORDER BY details.biographical.age
    LIMIT 2
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
