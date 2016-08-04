/* global exports */
"use strict";

var fs = require('fs');
var path = require('path');

// module Docopt.FFI

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
      } else {
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
    }
  }
};
