require('shelljs/global');

// Note: --merged needed to be made optional. The git help seems off here.
module.exports = (argv) => {
const args = require('../..').run(`

usage: git branch [options] [-r | -a] [--merged | --no-merged]
   or: git branch [options] [-l] [-f] <branchname> [<start-point>]
   or: git branch [options] [-r] (-d | -D) <branchname>...
   or: git branch [options] (-m | -M) [<oldbranch>] <newbranch>

Generic options:
    -v, --verbose         show hash and subject, give twice for upstream branch
    -q, --quiet           suppress informational messages
    -t, --track           set up tracking mode (see git-pull(1))
    --set-upstream        change upstream info
    -u, --set-upstream-to <upstream>
                          change the upstream info
    --unset-upstream      Unset the upstream info
    --color[=<when>]      use colored output
    -r, --remotes         act on remote-tracking branches
    --contains <commit>   print only branches that contain the commit
    --abbrev[=<n>]        use <n> digits to display SHA-1s

Specific git-branch options:
    -a, --all               list both remote-tracking and local branches
    -d, --delete            delete fully merged branch
    -D                      delete branch (even if not merged)
    -m, --move              move/rename a branch and its reflog
    -M                      move/rename a branch, even if target exists
    --list                  list branch names
    -l, --create-reflog     create the branch's reflog
    --edit-description      edit the description for the branch
    -f, --force             force creation (when already exists)
    --no-merged[=<commit>]  print only not merged branches
    --merged[=<commit>]     print only merged branches
    --column[=<style>]      list branches in columns

`, { argv: argv, smartOptions: true });

echo(JSON.stringify(args));
}

