const Ifrit = require("../dist")
const mongodb = require("mongodb")
const schema = require("./schema.json")

/*
 * Average power for mages under 170cm, by class
 */
const query = `
    SELECT AVG(spells_power) AS power
    FROM (
        SELECT AVG(spells.power), details.biographical.class AS class
        WHERE details.physical.height < 170
    )
    GROUP BY class
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
