import SpotLightNode from './SpotLightNode.js';

class IESSpotLightNode extends SpotLightNode {

  constructor( light = null ) {

    super( light );
    this._iesTextureNode = null;

  }

}

export default IESSpotLightNode;
