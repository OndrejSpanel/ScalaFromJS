function A(a) {
    this.a = a;
}

A.prototype.getA = function(){ return a; };

function AB(a, b) {
    A.call(a);
    this.b = b;

}

AB.prototype = new A;
AB.prototype.getB = function(){ return b; };

function C(c) {
    this.c = c;
}

C.prototype.getC = function(){ return c; };


function XYZ(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
}

XYZ.prototype.getX = function(){ return x; };
XYZ.prototype.getY = function(){ return y; };
XYZ.prototype.getZ = function(){ return z; };

new X(0);
new XY(0, 0);
new Z(0);
new XYZ(0, 0, 0);

function f() {
    var ca, cb, cab, cc, cxyz;
    var t;
    ca.a = 0;
    cab.a = ca.a;
    cb.b = ca.a;
    cab.b = 1;
    cxyz.x = 3;
    cxyz.z = 5;
    cc.c = cxyz.z;
    t.t = "";
}
