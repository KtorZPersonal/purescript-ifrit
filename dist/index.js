/**
 * @module Ifrit
 */

const { "Ifrit.Core": Ifrit, "Data.Either": Either } = require("./bundle.min")

/**
 * @namespace
 * @property {module:Ifrit~mongodb} mongodb - Compiler driver for mongodb
 */
exports.compile = {

    /**
     * @typedef {Function} mongodb
     * compiles an SQL-like query to a MongoDB aggregation pipeline query.
     *
     * @param {Object} schema - A JSON schema of the source collection
     * @param {String} query - The SQL-like query
     *
     * @return {Object[]}  The compiled result as a list of object
     * @throw {Error} Whenever a compilation error occurs
     */
    mongodb(schema, query){
        const result = Ifrit.compileMongoDB.compile(schema)(query)
        if (Object.getPrototypeOf(result) === Either.Left.prototype) {
            throw new Error(result.value0)
        }

        return result.value0
    }
}
