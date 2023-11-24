import React from "react";
import LoadingOverlay from "react-loading-overlay-ts";

export const loadingOverlayImpl = function(props) {
  return function(children) {
    return React.createElement(LoadingOverlay, props, ...children);
  };
};
