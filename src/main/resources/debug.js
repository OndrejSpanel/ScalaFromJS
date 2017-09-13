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
    else return "[" + name + "=" + value + "]";
};

UglifyJS.AST_DefClass.prototype.toDebug = function() {
    return this.name.name;
};

UglifyJS.AST_Symbol.prototype.toDebug = function() {
    if (this.thedef && this.thedef.orig.length > 0 && this.thedef.orig[0].start) {
        return this.thedef.name + ":" + this.thedef.orig[0].start.pos;
    }
    else {
        return this.name + ":-";
    }
};

UglifyJS.AST_Definitions.prototype.toDebug = function() {
    return this.definitions.join(",");
};

UglifyJS.AST_VarDef.prototype.toDebug = function() {
    return this.name.toString();
};

UglifyJS.AST_ConciseMethod.prototype.toDebug = function() {
    return this.key.name;
};

UglifyJS.AST_ObjectProperty.prototype.toDebug = function() {
    return this.key;
};

UglifyJS.AST_Defun.prototype.toDebug = function() {
    return this.name;
};
