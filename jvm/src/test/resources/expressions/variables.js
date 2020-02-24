function f()
{
    var s;
    var x;
    var y;
    var z;
    var u;

    s = "S";
    x = (function() {var a;a = 0;})();
    if (true) y = 1;
    if (u === undefined) u = "defined";
    var f = function() {
        y = "YY";
        z += 1;
        return x + s
    };
    if (true) {
        var l;
        l = 0;
    }
    y = "Y";
    z = 0;
    x++;
    return f() + x + s + y;
}
