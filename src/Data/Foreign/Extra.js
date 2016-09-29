/* global exports */
"use strict";

// module Data.Foreign.Extra

exports.isTruthy = function (value) {
  return !!value;
};

exports.undefined = undefined;

exports.toString = function(value) {
  return value.toString();
};
