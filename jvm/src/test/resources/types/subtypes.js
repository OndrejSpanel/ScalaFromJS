class X {}

class A extends X {}

class B extends X {}

function fx(px) {return px;}

function fa(pa) {return pa;}

function fb(pb) {return pb;}

var a = new A();
var ap = new A();

var b = new B();

var x = new X();

fx(a);
fx(b);

fa(a);
fb(b);

var xx, yy;
if (true) yy = fx(xx);

var xa;

fx(xa);
fa(xa);

var pp;
if (true) x = pp;
ap = pp;


