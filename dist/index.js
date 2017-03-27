/**
 * @module Ifrit
 */

const { "Ifrit.Core": Ifrit, "Data.Either": Either } = require("./bundle.min")

/**
 * Construct a new compilation result
 *
 * @constructor
 */
function Result(schema, output) {

    /**
     * The output schema of each document
     * @member {Object}
     */
    this.schema = schema

    /**
     * The compiled pipeline
     * @member {Array<Object>}
     */
    this.output = output
}


/**
 * get the corresponding JSON object output
 *
 * @return {Object} The compiled pipeline
 **/
Result.prototype.toJSON = function toJSON() {
    return this.output
}


/**
 * get the corresponding stringified output
 *
 * @return {String} The stringified compiled pipeline
 */
Result.prototype.toString = function toString() {
    return JSON.stringify(this.output)
}


/**
 * compiles an SQL-like query to a MongoDB aggregation pipeline query.
 *
 * @param {Object} schema - A JSON schema of the source collection
 * @param {String} query - The SQL-like query
 * @param {module:Ifrit~CompileCallback} [cb] - An optional callback, @see @return
 *
 * @return {Undefined|Promise<module:Ifrit~Result, Error>}  A promise eventually resolved
 *  with a result. It return nothing when a callback is provided
 */
exports.compile = function compile(schema, query, cb) {
    const isCallback = typeof cb === "function"

    try {
        const result = exports.compileSync(schema, query)

        if (isCallback) {
            cb(null, result)
            return
        }

        return Promise.resolve(result)
    } catch (err) {
        if (isCallback) {
            /**
             * @callback CompileCallback
             *
             * @param {Null|Error} err - An error if any, null otherwise
             * @param {module:Ifrit~Result} The corresponding result
             */

            cb(err)
            return
        } else {
            return Promise.reject(err)
        }
    }
}


/**
 * compiles an SQL-like query to a MongoDB aggregation pipeline query.
 *
 * @param {Object} schema - A JSON schema of the source collection
 * @param {String} query - The SQL-like query
 *
 * @return {module:Ifrit~Result} The corresponding result
 * @throw {Error} Whenever something goes wrong during the compilation
 */
exports.compileSync = function compileSync(schema, query) {
    const result = Ifrit.compileMongoDB.compile(schema)(query)
    if (Object.getPrototypeOf(result) === Either.Left.prototype) {
        throw new Error(result.value0)
    }

    return new Result(result.value0.schema, result.value0.output)
}
