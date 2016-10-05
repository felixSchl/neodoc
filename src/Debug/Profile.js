/* global exports */
"use strict";

// module Debug.Profile

const _timers = {};

exports.timerStart = function () {
  return process.hrtime();
}

exports.timerEnd = function (start) {
  return function () {
    return process.hrtime(start)[1]/1000000;
  };
}
