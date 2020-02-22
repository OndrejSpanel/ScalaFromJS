// inspired by Stravistix Background.js

var Background = (function () {
    function Background() {
    }
    Background.prototype.init = function () {
    };
    return Background;
}());
var background = new Background();
background.init();

// inspired by various Three.js classes

function BoxGeometry( width, height ) {

    this.type = 'BoxGeometry';
}

BoxGeometry.prototype = Object.create( Geometry.prototype );
BoxGeometry.prototype.constructor = BoxGeometry;

function HemisphereLight( skyColor, groundColor, intensity ) {

    this.type = 'HemisphereLight';
}

HemisphereLight.prototype = Object.assign( Object.create( Light.prototype ), {

    constructor: HemisphereLight,

    isHemisphereLight: true,
} );


function Clock( autoStart ) {

}

Object.assign( Clock.prototype, {

    start: function () {
    },

    stop: function () {
    },

} );


var object3DId = 0;

function Object3D() {

    Object.defineProperty( this, 'id', { value: object3DId ++ } );

}

Object3D.DefaultUp = new Vector3( 0, 1, 0 );
Object3D.DefaultMatrixAutoUpdate = true;

Object.assign( Object3D.prototype, EventDispatcher.prototype, {

    isObject3D: true,

    setRotationFromEuler: function ( euler ) {

        this.quaternion.setFromEuler( euler, true );

    },
} );

