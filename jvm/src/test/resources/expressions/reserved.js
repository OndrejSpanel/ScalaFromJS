
function Animal(type) {
    this.type = type;
};
Animal.prototype.move = function(meters) {
};

Animal.prototype.match = function(val) {return val === this ? this : undefined;};

function Snake() {
    Animal.apply(this, Array.prototype.slice.call(arguments));
};
Snake.prototype = new Animal();
Snake.prototype.move = function() {
    console.log("Slithering...");
    Animal.prototype.move.call(this, 5);
};

var object = new Snake("Sammy the Python");
object.move();
object.match(x).move();

function lazy(object, meters) {
    object.move(meters * 0.1);
}