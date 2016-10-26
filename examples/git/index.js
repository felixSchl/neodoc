require('shelljs/global');

const neodoc = require('../..');
const args = neodoc.run(`
usage: git [--version] [--help] [-C <path>] [-c <name=value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           [<command> [<args>]...]

The most commonly used git commands are:
   add        Add file contents to the index
   bisect     Find by binary search the change that introduced a bug
   branch     List, create, or delete branches
   checkout   Checkout a branch or paths to the working tree
   clone      Clone a repository into a new directory
   commit     Record changes to the repository
   diff       Show changes between commits, commit and working tree, etc
   fetch      Download objects and refs from another repository
   grep       Print lines matching a pattern

'git help -a' and 'git help -g' lists available subcommands and some
concept guides. See 'git help <command>' or 'git help <concept>'
to read about a specific subcommand or concept.
`, { optionsFirst: true, smartOptions: true, version: '1.0.0', versionFlags: ['--info-path'] });

if (args['<command>']) {
  require(`./${args['<command>']}`)(
    [args['<command>']].concat(args['<args>']));
} else {
  echo(JSON.stringify(args));
}
