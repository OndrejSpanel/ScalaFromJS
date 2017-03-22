function Cls(x, y){
    this.x = x;
    this.y = y;
}

Cls.defX = 0;
Cls.defY = function() {return 0;};

Cls.prototype.set = function(x, y) {
    this.x = x || Cls.defX;
    this.y = y || Cls.defY();
};

var Utils = {

    pi: 3.14,

    funA: function (a) {
        return a;
    },

    funB: function (a, b) {
        return a + b;
    }
};

var aPi = Utils.funA(Utils.pi);
