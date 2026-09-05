import AnalyticLightNode from './AnalyticLightNode.js';

class SpotLightNode extends AnalyticLightNode {

  constructor( light = null ) {

    super( light );
    this.coneCosNode = 0;

  }

}

export default SpotLightNode;
