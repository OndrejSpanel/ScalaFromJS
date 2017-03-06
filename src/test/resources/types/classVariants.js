// inspired by Stravistix Background.js

var Background = (function () {
    function Background() {
    }
    Background.prototype.init = function () {
        this.listenForExternalMessages();
        this.listenInstallUpdate();
    };
    return Background;
}());
var background = new Background();
background.init();
