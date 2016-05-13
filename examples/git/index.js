require('shelljs/global');

const args = require('../..').run(`
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

'git help -a' and 'git help -g' lists available subcommands and some
concept guides. See 'git help <command>' or 'git help <concept>'
to read about a specific subcommand or concept.
`, { optionsFirst: true, smartOptions: true });

if (args['<command>']) {
  require(`./${args['<command>']}`)(
    args['<command>'].concat(args['<args>']));
} else {
  echo(args);
}
