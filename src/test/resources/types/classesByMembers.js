function X(x) {
    this.x = x;
}

X.prototype.getX = function(){ return x; };

function XY(x, y) {
    X.call(x);
    this.y = y;

}

XY.prototype = new X;
XY.prototype.getY = function(){ return y; };

function Z(z) {
    this.z = z;
}

Z.prototype.getZ = function(){ return z; };


function XYZ(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
}

XYZ.prototype.getX = function(){ return x; };
XYZ.prototype.getY = function(){ return y; };
XYZ.prototype.getZ = function(){ return z; };



function f() {
    var cx, cy, cxy, cz, cxyz;
    var t;
    cx.x = 0;
    cxy.x = cx.x;
    cy.y = cx.x;
    cxy.y = 1;
    cxyz.x = 3;
    cxyz.z = 5;
    cz.z = cxyz.z;
    t.t = "";
}
