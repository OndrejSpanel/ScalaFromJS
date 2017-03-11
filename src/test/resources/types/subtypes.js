class X {}

class A extends X {}

class B extends X {}

function fx(p) {
    return p;
}

var a = new A();

var b = new B();

var x = new X();

fx(a);
fx(b);

var xx, yy;
if (true) yy = fx(xx);

var pp;
if (true) x = pp;
a = pp;


