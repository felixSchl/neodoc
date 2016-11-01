/**
 * Run the compiled neodoc output through the google closure compiler.
 * This carves another 15ms of requiring *and* running neodoc (w/ git example)
 */

var fs = require('fs');
var compile = require('google-closure-compiler-js').compile;

var input = fs.readFileSync('./dist/neodoc.purs.js', 'utf8');

console.log('[optimize] preparing neodoc output for closure compiler...')
var jsCode = input.replace(/(exports|PS)\.([a-zA-Z-$_0-9]+)/g, function(_,o,k) {
  return o + '["' + k + '"]';
});

console.log('[optimize] running closure compiler on neodoc output')
var output = compile({
  jsCode: [{
    src: jsCode,
    compilationLevel: 'ADVANCED',
  }],
}).compiledCode;

fs.writeFileSync('./dist/neodoc.purs.min.js', output);
