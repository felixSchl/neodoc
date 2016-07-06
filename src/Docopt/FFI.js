/* global exports */
"use strict";

// module Docopt.FFI

exports.isTruthy = function (value) {
  return !!value;
};

exports.undefined = undefined;

exports.toString = function(value) {
  return value.toString();
}
