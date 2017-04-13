function objectGetName(o) {
    if (o === undefined) return undefined;
    if (o.CTOR) return o.CTOR.name;
    if (o.constructor) return o.constructor.name;
    return undefined;
}

Object.prototype.toString = function () {
    var name = objectGetName(this);
    var value = this.toDebug ? this.toDebug() : "";
    if (!name) return "[unknown object]";
    else return "[object " + name + "=" + value + "]";
};

UglifyJS.AST_DefClass.prototype.toDebug = function() {
    return this.name.name;
};

