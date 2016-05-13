require('shelljs/global');

module.exports = (argv) => {
const args = require('../..').run(`
usage: git add [options] [--] <pathspec>...

Options:
  -n, --dry-run         dry run
  -v, --verbose         be verbose

  -i, --interactive     interactive picking
  -p, --patch           select hunks interactively
  -e, --edit            edit current diff and apply
  -f, --force           allow adding otherwise ignored files
  -u, --update          update tracked files
  -N, --intent-to-add   record only the fact that the path will be added later
  -A, --all             add changes from all tracked and untracked files
  --ignore-removal      ignore paths removed in the working tree (same as --no-all)
  --refresh             don't add, only refresh the index
  --ignore-errors       just skip files which cannot be added because of errors
  --ignore-missing      check if - even missing - files are ignored in dry run
`, { argv: argv, smartOptions: true });

echo(args);
}
