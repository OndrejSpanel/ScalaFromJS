//@example
import {C} from "src/c.js"
export {A} from "src/a.js"
export * from "src/b.js"

var ScalaFromJS_settings = {
    packages: [
        {
            folder: "src",
            name: "my.name",
            operation: "name"
        },
        {
            folder: "test",
            name: "my.name",
            operation: "name"
        },

    ]
};