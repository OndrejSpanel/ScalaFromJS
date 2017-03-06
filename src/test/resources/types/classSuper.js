
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

var sam = new Snake("Sammy the Python");
sam.move();
