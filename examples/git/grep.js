require('shelljs/global');

module.exports = (argv) => {
const args = require('../..').run(`

usage: git grep [options] <pattern> [<rev>...] [[--] <path>...]

options:
    --cached              search in index instead of in the work tree
    --no-index            find in contents not managed by git
    --untracked           search in both tracked and untracked files
    --exclude-standard    search also in ignored files

    -v, --invert-match    show non-matching lines
    -i, --ignore-case     case insensitive matching
    -w, --word-regexp     match patterns only at word boundaries
    -a, --text            process binary files as text
    -I                    don't match patterns in binary files
    --textconv            process binary files with textconv filters
    --max-depth <depth>   descend at most <depth> levels

    -E, --extended-regexp
                          use extended POSIX regular expressions
    -G, --basic-regexp    use basic POSIX regular expressions (default)
    -F, --fixed-strings   interpret patterns as fixed strings
    -P, --perl-regexp     use Perl-compatible regular expressions

    -n, --line-number     show line numbers
    -h                    don't show filenames
    -H                    show filenames
    --full-name           show filenames relative to top directory
    -l, --files-with-matches
                          show only filenames instead of matching lines
    --name-only           synonym for --files-with-matches
    -L, --files-without-match
                          show only the names of files without match
    -z, --null            print NUL after filenames
    -c, --count           show the number of matches instead of matching lines
    --color[=<when>]      highlight matches
    --break               print empty line between matches from different files
    --heading             show filename only once above matches from same file

    -C, --context <n>     show <n> context lines before and after matches
    -B, --before-context <n>
                          show <n> context lines before matches
    -A, --after-context <n>
                          show <n> context lines after matches
    -p, --show-function   show a line with the function name before matches
    -W, --function-context
                          show the surrounding function

    -f <file>             read patterns from file
    -e <pattern>          match <pattern>
    --and                 combine patterns specified with -e
    --or
    --not
    (
    )
    -q, --quiet           indicate hit with exit status without output
    --all-match           show only matches from files that match all patterns

    -O, --open-files-in-pager[=<pager>]
                          show matching files in the pager
    --ext-grep            allow calling of grep(1) (ignored by this build)

`, { argv: argv, smartOptions: true });

echo(JSON.stringify(args));
}
