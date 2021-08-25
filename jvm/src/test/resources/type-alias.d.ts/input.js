class D {
    ab() {
        return new C().getAB()
    }
    abEx() {
        return new C().getAB()
    }
}

var dab;
var dabEx;

if (true) {
    dab = new D().ab();
    dabEx = new D().abEx();
}

export { D };

var ScalaFromJS_settings = {
    types: "input.d.ts"
};
