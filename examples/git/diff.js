require('shelljs/global');

module.exports = (argv) => {
const args = require('../..').run(`
usage: git diff [<options>] [<commit> [<commit>]] [--] [<path>...]
`, { argv: argv, smartOptions: true });

echo(args);
}

