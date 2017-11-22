//@example
import {C} from "src/c.js"
export {A} from "src/a.js"
export * from "src/b.js"
export * from "test/Some.tests"

var ScalaFromJS_settings = {
    packages: [
        {
            folder: "test",
            operation: "name",
            name: "my.name/tst" ,
            template: [
                "class ${class}Test extends Tests {",
                "import something.{assert => ok}",
                "",
                "$this",
                "// end",
                "}"
            ]
        },
        {
            folder: "src",
            operation: "name",
            name: "my.base"
        },

    ]
};