const $ = require('shelljs');

const NODE_CMD = $.which('node');

/**
 * Execute a node script in a separate process
 */
module.exports.exec = function (script, args) {
  const p = $.exec(`"${NODE_CMD}" "${script}" ${args}`, { silent: true });
  if (p.code === 0) {
    return p.stdout;
  } else {
    throw new Error(p.stdout || p.stderr);
  }
}

/**
 * Run a fake process.
 *
 * `console.log` is captured as stdout
 * `console.error` is captured as stderr
 * `process.exit` throws an error
 *
 * Note: it's crude but works in the context of neodoc since all operations
 * are synchronous and tests are run in series.
 */
module.exports.runFakeProc = function (f) {
  let code = 0;
  const stderr = [], stdout = [];
  const clog = console.log, cerror = console.error, pexit = process.exit;
  console.log = stdout.push.bind(stdout);
  console.error = stderr.push.bind(stderr);

  process.exit = (code) => {
    var err = new Error();
    err.exitCode = code;
    throw err;
  };

  try {
    f();
  } catch(e) {
    if (typeof e.exitCode !== 'undefined') {
      code = e.exitCode;
    } else {
      console.log(e);
      throw e;
    }
  } finally {
    console.log = clog;
    console.error = cerror;
    process.exit = pexit;
  }
  return {
    stderr: stderr.join('\n')
  , stdout: stdout.join('\n')
  , code
  };
}

