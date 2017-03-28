const Benchmark = require("benchmark")
const Ifrit = require("dist")

let suite = new Benchmark.Suite()

suite
    .add('SELECT age', function() {
        Ifrit.compileSync({
            "age": "number",
            "class": "string",
            "is_master": "boolean",
            "bonus": ["number"],
            "spells": [{
                "name": "string",
                "power": "number"
            }]
        }, "SELECT age")
    })

    .add('SELECT class AS klass, COUNT(bonus)', function() {
        Ifrit.compileSync({
            "age": "number",
            "class": "string",
            "is_master": "boolean",
            "bonus": ["number"],
            "spells": [{
                "name": "string",
                "power": "number"
            }]
        }, "SELECT class AS klass, COUNT(bonus)")
    })

    .add('SELECT AVG(age) GROUP BY class', function() {
        Ifrit.compileSync({
            "age": "number",
            "class": "string",
            "is_master": "boolean",
            "bonus": ["number"],
            "spells": [{
                "name": "string",
                "power": "number"
            }]
        }, "SELECT AVG(age) GROUP BY class")
    })

    .add('SELECT is_master WHERE age > 14 AND age < 20', function() {
        Ifrit.compileSync({
            "age": "number",
            "class": "string",
            "is_master": "boolean",
            "bonus": ["number"],
            "spells": [{
                "name": "string",
                "power": "number"
            }]
        }, "SELECT is_master WHERE age > 14 AND age < 20")
    })

    .add('SELECT AVG(power) AS avg_power FROM (SELECT AVG(spells.power), age) WHERE age > 18 GROUP BY NULL', function() {
        Ifrit.compileSync({
            "age": "number",
            "class": "string",
            "is_master": "boolean",
            "bonus": ["number"],
            "spells": [{
                "name": "string",
                "power": "number"
            }]
        }, "SELECT AVG(power) AS avg_power FROM (SELECT AVG(spells.power), age) WHERE age > 18 GROUP BY NULL")
    })

    .on('cycle', function(event) {
      console.log(String(event.target));
    })

    .run()
