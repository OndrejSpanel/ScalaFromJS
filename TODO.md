`DefineProperties`:

```


function Animal(name) {
    this.name = name;
};
Animal.prototype.move = function(meters) {
    console.log(this.name+" moved "+meters+"m.");
};

function Snake() {
    Animal.apply(this, Array.prototype.slice.call(arguments));
};
Snake.prototype = new Animal();
Snake.prototype.move = function() {
    console.log("Slithering...");
    Animal.prototype.move.call(this, 5);
};

Snake.prototype.advance = function() {
    var tempX = 0;
    var tempY = "";
    return function (dist) {
        tempX = dist;
        tempY = tempX.toString();
        move();
    }

}();

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

```