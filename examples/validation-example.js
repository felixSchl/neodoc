require('shelljs/global');
const _ = require('lodash');
const Schema = require('js-schema');
const File = require('./schema-extensions/File');
const Dir = require('./schema-extensions/Dir');

const args = require('..').run(`
  Usage: prog.js [--count=N] PATH FILE...

  Arguments:
    FILE          input file
    PATH          out directory

  Options:
    --count=N     number of operations
`);

const errors = Schema({
  FILE:       File.exists()
, PATH:       Dir.exists()
, '?--count': Number.min(0).max(5)
}).errors(args);

if (errors) {
  _.each(errors, (error, arg) => {
    echo(`Invalid ${arg} argument: ${error}`);
  })
  exit(1)
}

echo(args);
