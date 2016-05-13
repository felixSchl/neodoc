require('shelljs/global');
const _ = require('lodash');

// The git-bisect help text is a bit unconventional and requires some special
// treatment for prepartion
// XXX: -- should be [--]. See #31
module.exports = (argv) => {

const help = `
usage: git bisect [help|start|bad|good|skip|next|reset|visualize|replay|log|run] [<args>...]

git bisect help
    print this long help message.
git bisect start [--no-checkout] [<bad> [<good>...]] -- [<pathspec>...]
    reset bisect state and start bisection.
git bisect bad [<rev>]
    mark <rev> a known-bad revision.
git bisect good [<rev>...]
    mark <rev>... known-good revisions.
git bisect skip [(<rev>|<range>)...]
    mark <rev>... untestable revisions.
git bisect next
    find next bisection to test and check it out.
git bisect reset [<commit>]
    finish bisection search and go back to commit.
git bisect visualize
    show bisect status in gitk.
git bisect replay <logfile>
    replay bisection log.
git bisect log
    show bisect log.
git bisect run <cmd>...
    use <cmd>... to automatically bisect.

Please use "git help bisect" to get the full man page.
`

const args = require('../..').run(help);

// The `git bisect -h` output is a bit unconventional, so this is not really
// examplary of how to use neodoc idiomatically. Key is: if it's really
// required, with a bit of help, neodoc can do it.
const cmds = _.fromPairs(_.map(
  _.chunk(_.dropRight(_.drop(help.split('\n'), 3), 3), 2)
, xs => {
  const cmd = xs[0].split(' ')[2];
  return [ cmd, `usage: ${xs[0]}\n\n${xs[1].trim()}]}` ]
}));
const cmd = _.keys(_.omit(args, 'bisect', '<args>', 'ARGS'))[0];
if (_.has(cmds, cmd)) {
  const cargs = require('../..').run(
    cmds[cmd]
  , { argv: [ 'bisect', cmd ].concat(args['<args>']) }
  );
  echo(cargs);
} else if (cmd) {
  echo(`Unknown git bisect command: ${cmd}`);
  exit(1);
} else {
  echo(args);
}
}
