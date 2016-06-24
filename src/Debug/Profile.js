/* global exports */
"use strict";

// module Debug.Profile

exports.sampleTime = function() {
  return process.hrtime()[1] / 1000000;
};
