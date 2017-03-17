Type inference

```
function Animal(name) {
    this.name = name;
};
function Snake() {
    Animal.apply(this, Array.prototype.slice.call(arguments));
};
Snake.prototype = new Animal();
Snake.prototype.isSnake = true;

var sam = new Snake("Sammy the Python");
sam.move();

Object.defineProperties( Animal.prototype, {

    fullName: {

        get: function () {

            return this.name;

        },

        set: function ( value ) {

            this.name = value;
            this.onChangeCallback();

        }

    },
});

var x, y;
if (true) x = sam.fullName
if (true) y = sam.isSnake



```