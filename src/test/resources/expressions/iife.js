var a = function () {
    var x;
    x = "Hello";
    return x;
}();

var f = function(){
    return function() {
        return 0;
    }
}();

function Functions() {

}

Functions.prototype.a = function() {
    return function() {
        return "X";
    }
}();


Functions.prototype.b = function() {
    var temp = new Temporary()
    return function() {
        return "X";
    }
}();
