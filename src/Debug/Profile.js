/* global exports */
"use strict";

// module Debug.Profile

const _timers = {};

exports.timerStart = function () {
  return process.hrtime();
}

exports.timerEnd = function (start) {
  return function () {
    var hrTime = process.hrtime(start);
    return hrTime[0] * 1000 + hrTime[1] / 1000000
  };
}
