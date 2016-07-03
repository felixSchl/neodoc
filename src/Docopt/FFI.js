/* global exports */
"use strict";

// module Docopt.FFI

exports.isTruthy = function (value) {
  return (value == true);
};

exports.undefined = undefined;

exports.toString = function(value) {
  return value.toString();
}
