class D {
    f(a) {
        var ds;
        var dn;
        if (true) {
            ds = a[0];
            dn = a.length;
        }
    }
}

/* Scala: // TODO: parse source comments for requirements
require:
dn: Double
ds: String
*/

var SNamespace = {
    sVarN: 1.0,

    sGenS: function () {return "s"},
    sCompute: function(x){return x;},
    sInfer: function(sns){return sns === "SNS";}
};
