//@example
import {C} from "src/c.js"
export {A} from "src/a.js"
export * from "src/b.js"
export * from "test/Some.tests"
import dd from "src/data/dd.data.js";

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
                "}",
                ""
            ]
        },
        {
            folder: "src/data",
            operation: "name",
            name: "my.base.data" ,
            template: [
                "object ${class} extends Data {",
                "val value = $this",
                "}",
                ""
            ]
        },
        {
            folder: "src",
            operation: "name",
            name: "my.base"
        },

    ]
};