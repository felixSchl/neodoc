require('shelljs/global');

const args = require('..').run(`
Usage: odd_even_example.py [-h | --help] (ODD EVEN)...

Example, try:
  odd_even_example.py 1 2 3 4

Options:
  -h, --help
`);

echo(args);
