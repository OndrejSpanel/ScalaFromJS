function f(a,b,c,d) {
    var x = b !== undefined ? b : 2;
    if (c === undefined) c = 1;
    var z = d || 0;
}

function g(x,y) {
    y = y || ""
}

function Cls() {}

Cls.prototype.f = function ff(aa, bb, cc, dd) {
    var xx = bb !== undefined ? bb : 2;
    if (cc === undefined) cc = 1;
    var zz = dd || 0;
    return aa + xx + cc + zz
};
