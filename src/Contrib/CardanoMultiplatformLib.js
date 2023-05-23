import browserOrNode from 'browser-or-node';

export const importLibImpl = function() {
  if(browserOrNode.isNode) {
    console.log("RETURNING NODE LIB");
    return import('@dcspark/cardano-multiplatform-lib-nodejs');
  } else if(browserOrNode.isBrowser) {
    console.log("RETURNING BROWSER LIB");
    return import('@dcspark/cardano-multiplatform-lib-browser');
  }
  return null;
};
