"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports["default"] = void 0;
function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }
function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }
function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, _toPropertyKey(descriptor.key), descriptor); } }
function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }
function _defineProperty(obj, key, value) { key = _toPropertyKey(key); if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }
function _toPropertyKey(arg) { var key = _toPrimitive(arg, "string"); return _typeof(key) === "symbol" ? key : String(key); }
function _toPrimitive(input, hint) { if (_typeof(input) !== "object" || input === null) return input; var prim = input[Symbol.toPrimitive]; if (prim !== undefined) { var res = prim.call(input, hint || "default"); if (_typeof(res) !== "object") return res; throw new TypeError("@@toPrimitive must return a primitive value."); } return (hint === "string" ? String : Number)(input); }
var GlobalStateManager = /*#__PURE__*/function () {
  function GlobalStateManager() {
    _classCallCheck(this, GlobalStateManager);
    _defineProperty(this, "state", void 0);
    this.state = {};
  }
  _createClass(GlobalStateManager, [{
    key: "appendValue",
    value: function appendValue(key, incredmentalValue) {
      this.state[key] = this.state[key] || [];
      this.state[key].push(incredmentalValue);
    }
  }, {
    key: "popValue",
    value: function popValue(key) {
      return this.state[key].pop();
    }
  }, {
    key: "getValue",
    value: function getValue(key) {
      var values = this.state[key];
      return values[values.length - 1];
    }

    // method to get value by key
  }, {
    key: "get",
    value: function get(key) {
      return this.state[key];
    }

    // method to set value by key
  }, {
    key: "set",
    value: function set(key, value) {
      this.state[key] = value;
    }
  }]);
  return GlobalStateManager;
}();
var _default = GlobalStateManager;
exports["default"] = _default;