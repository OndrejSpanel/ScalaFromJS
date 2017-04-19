function C() {

}

C.prototype.constructor = C;

C.prototype.naturalFunction = function() {
    this.naturalMember = 0;
    this.exoticMember = 0;
};

C.prototype.exoticFunction = function() {};

var ScalaFromJS_settings = {
    members: [
        {
            cls: ".*",
            name: "exotic.*",
            operation: "delete"
        },
    ]
};
