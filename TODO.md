Color r g b

```
function Color( r, g, b ) {


	return this.setRGB( r, g, b );

}

Color.prototype = {

	constructor: Color,

	isColor: true,

	r: 1, g: 1, b: 1,

	setRGB: function ( r, g, b ) {

		this.r = r;
		this.g = g;
		this.b = b;

		return this;

	},


};


```