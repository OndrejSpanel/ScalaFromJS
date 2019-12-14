class A {

}

function a(x) { // x defined in dt.s as string
    x = new A();
    return x;
}

var r;
var m;
var bnum;
var bstr;
var xnum;

if (true) {
    var ca = new A;
    r = a(0);
    m = ca.member; // member defined in dt.s as number

    var cb = new B;
    bnum = cb.num;
    bstr = cb.str;
    xnum = bnum;
}
