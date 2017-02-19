function f()
{
    var s = "S";
    var x = 0;
    var y = "Y";
    var z = 0;
    x++;
    var f = function() {
        y = "YY";
        z += 1;
        return x + s
    };
    return f() + x + s + y;
}
