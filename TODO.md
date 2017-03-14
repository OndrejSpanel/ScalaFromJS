V následujícím kódu špatně funguje inference pro `this.vvv`, proto také nejde použít definice `numberFunction` pro
typ ve `stdLibraryMembers`. Podobně nefunguje inference pro `this.vv`:

```function Vector3( x, y, z ) {

    this.x = x || 0;
    this.y = y || 0;
    this.z = z || 0;

}

Object.assign( Vector3.prototype, {

    set: function ( x, y, z ) {

        this.xx = Math.sin(x);
        this.yy = Math.abs(y);
        this.zz = Math.ceil(z);

        this.v = function(a){}
        this.v(0)
        this.vvv = this.v(this.vv)

        return this;

    },
} );
```