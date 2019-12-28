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

    get gs(){return this._gs;}
    set gs(b){this._gs = b;}
    static f(a, b){return b;}
    static g(a, b){return b;}
}

class SObj {
    static sfa(){return "";}
    static sfb(){return 0;}
}

function createCSI() {
    return new CS();
}
