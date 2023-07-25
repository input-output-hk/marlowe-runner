"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getElementLocator = void 0;
var _navigationBehavior = require("./navigation-behavior");
var getElementLocator = function getElementLocator(page, elementKey, globalConfig) {
  var _pageElementMappings$, _pageElementMappings$2, _pageElementMappings$3;
  var currentPage = (0, _navigationBehavior.getCurrentPageId)(page, globalConfig);
  console.log("currentPage: ".concat(currentPage));
  var pageElementMappings = globalConfig.pageElementMappings;
  return ((_pageElementMappings$ = pageElementMappings[currentPage]) === null || _pageElementMappings$ === void 0 ? void 0 : _pageElementMappings$[elementKey]) || ((_pageElementMappings$2 = pageElementMappings[currentPage]) === null || _pageElementMappings$2 === void 0 ? void 0 : (_pageElementMappings$3 = _pageElementMappings$2.common) === null || _pageElementMappings$3 === void 0 ? void 0 : _pageElementMappings$3[elementKey]);
};
exports.getElementLocator = getElementLocator;