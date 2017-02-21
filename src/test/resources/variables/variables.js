function f()
{
    var s;
    var x;
    var y;
    var z;

    s = "S";
    x = 0;
    if (true) y = 1;
    var f = function() {
        y = "YY";
        z += 1;
        return x + s
    };
    y = "Y";
    z = 0;
    x++;
    return f() + x + s + y;
}
