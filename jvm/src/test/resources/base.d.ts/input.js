class A {
    f () {}
    g(x) {return x}
    fa(x, y) {}
}

class B {
    f () {}
    g(x) {return x}
    fb(x, y) {}
}

class C {
    f () {}
}

var i1;
var i2;

if (true) {
    i1 = new A();
    i1 = new B();
    i1 = new C();

    i2 = new A();
    i2 = new B();
}

var ScalaFromJS_settings = {
    types: "input.d.ts"
};
