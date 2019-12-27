import {CSI} from "./c";

class C {
    constructor(cn, cs, cb) {
        this.cn = cn;
        this.cs = cs;
        this.cb = cb;
    }
}

class CX {
    constructor(cn, cs, cb) {
    }
}

class CS {
    f(a){return a;}
    g(a){return a;}

    static f(a, b){return b;}
    static g(a, b){return b;}
}

function createCSI() {
    return new CS();
}
