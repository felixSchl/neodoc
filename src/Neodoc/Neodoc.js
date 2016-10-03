/* global exports */
"use strict";

// module Neodoc

var fs = require('fs');
var path = require('path');

function NeodocError(message, payload) {
  this.message = message;
  this.payload = payload;
  Error.call(this);
}

NeodocError.prototype.name = 'NeodocError';
NeodocError.prototype = Object.create(Error);

exports.jsError = function(msg) {
  return function(payload) {
    return new NeodocError(msg, payload);
  };
};

exports.undefined = undefined;

exports.toString = function(value) {
  return value.toString();
}

/**
 * Try and detect the version as indicated in the package.json neighbouring
 * the main module. Uses `require.main` to detect the main module and traverses
 * the parent directories in a search for a `package.json` using
 * `require.main.paths`.
 */

exports.readPkgVersionImpl = function (Just) {
  return function(Nothing) {
    return function() {
      if (!require.main) {
        return Nothing;
      } else if (require.main && require.main.paths /* in node? */){
        for (var i=0; i < require.main.paths.length; i++) {
          var xs = require.main.paths[i].split(path.sep);
          if (xs.pop() === 'node_modules' && xs.length > 1) {
            xs.push('package.json');
            var p = xs.join(path.sep);
            if (fs.existsSync(p)) {
              return Just(JSON.parse(fs.readFileSync(p)).version);
            }
          }
        }
        return Nothing;
      }
      return Nothing;
    }
  }
};

exports.runUnsafeJSCallbackOnce = function(cb) {
  var hasRun = false;
  var lastResult = null;
  return function (arg) {
    if (hasRun) { return lastResult; }
    hasRun = true;
    lastResult = cb(arg)();
    console.log(hasRun, lastResult);
    return lastResult
  };
}
