/* global exports */
"use strict";

// module Debug.Profile

var _timers = {};

exports._ENABLE_PROFILING_ = process.env['NEODOC_ENABLE_PROFILE'] == '1' ||
                              process.env['NEODOC_ENABLE_PROFILE'] == 'true';

exports.timerStart = function () {
  return process.hrtime();
}

exports.timerEnd = function (start) {
  return function () {
    var hrTime = process.hrtime(start);
    return hrTime[0] * 1000 + hrTime[1] / 1000000
  };
}
