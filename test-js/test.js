const _ = require('lodash');
const neodoc = require('../');
const path = require('path');
const chalk = require('chalk');
const expect = require('chai').expect;

const EXAMPLES = path.resolve(__dirname, '..', 'examples');
const GIT_EXAMPLE = path.resolve(EXAMPLES, 'git');

const { runFakeProc, exec } = require('./support');

describe('neodoc', () => {
  describe('examples - git', () => {
    const git = (args) => {
      const output = exec(GIT_EXAMPLE, args)
      return JSON.parse(output);
    }

    it('git branch -u origin master', () => {
      expect(git('branch -u origin master'))
        .to.deep.equal({
          branch: true
        , '<branchname>': ['master']
        , '-u': 'origin'
        , '--set-upstream-to': 'origin'
        });
    });

    it('git branch -D master development', () => {
      expect(git('branch -D master development'))
        .to.deep.equal({
          branch: true
        , '<branchname>': [ 'master', 'development' ]
        , '-D': true
        });
    });

    it('git clone --separate-git-dir=~/foo', () => {
      expect(git('clone --separate-git-dir=~/foo'))
        .to.deep.equal({
          '--': []
        , '--separate-git-dir': '~/foo'
        , 'clone': true
        });
    });

    it('git commit --gpg-sign=my-key', () => {
      expect(git('commit --gpg-sign=my-key'))
        .to.deep.equal({
          '--': []
        , 'commit': true
        , '--gpg-sign': 'my-key'
        , '-S': 'my-key'
        });
    });
  });

  describe('special arguments', () => {
    describe('option.helpFlags', () => {
      it('should return the help', () => {
        const help = 'usage: p [--help]';
        const out = neodoc.run(help, { dontExit: true, argv: [ '--help' ]});
        expect(out).to.deep.equal({"--help": 1, ".help": help });
      });

      it('should return the help (implicit)', () => {
        const help = 'usage: p';
        const out = neodoc.run(help, { dontExit: true, argv: [ '--help' ]});
        expect(out).to.deep.equal({"--help": 1, ".help": help });
      });

      it('should return the help (implicit, should not fail)', () => {
        const help = 'usage: p <word>';
        const out = neodoc.run(help, { dontExit: true, argv: [ '--help' ]});
        expect(out).to.deep.equal({"--help": 1, ".help": help });
      });

      it('should return the help (implicit w/ alias)', () => {
        const help = `\
          usage: p
          options:
            -h, --help`;
        const out = neodoc.run(help, { dontExit: true, argv: [ '-h' ]});
        expect(out).to.deep.equal({"--help": 1, "-h": 1, ".help": help });
      });

      it('should accept custom helpFlags (implicit)', () => {
        const help = 'usage: p'
        const out = neodoc.run(help, { dontExit: true, argv: [ '-h' ], helpFlags: [ '-h' ]});
        expect(out).to.deep.equal({"-h": 1, ".help": help });
      });
    });

    describe('option.versionFlags', () => {
      it('should return the version', () => {
        const version = '1.0.0';
        const help = 'usage: p [--version]';
        const out = neodoc.run(help, { dontExit: true, argv: [ '--version' ], version });
        expect(out).to.deep.equal({"--version": 1, ".version": version });
      });

      it('should return the version (implicit)', () => {
        const version = '1.0.0';
        const help = 'usage: p';
        const out = neodoc.run(help, { dontExit: true, argv: [ '--version' ], version});
        expect(out).to.deep.equal({"--version": 1, ".version": version });
      });

      it('should return the version (implicit w/ alias)', () => {
        const version = '1.0.0';
        const help = `\
          usage: p
          options:
            -v, --version`;
        const out = neodoc.run(help, { dontExit: true, argv: [ '-v' ], version});
        expect(out).to.deep.equal({"--version": 1, "-v": 1, ".version": version });
      });

      it('should accept custom versionFlags (implicit)', () => {
        const version = '1.0.0';
        const help = 'usage: p'
        const out = neodoc.run(help, { dontExit: true, argv: [ '-v' ], versionFlags: [ '-v' ], version});
        expect(out).to.deep.equal({"-v": 1, ".version": version });
      });
    });
  });

  describe('special arguments (via cli)', () => {
    describe('option.helpFlags', () => {
      it('should print help', () => {
        for (let flag of ['-h', '--help', '-?']) {
          const result = runFakeProc(() => {
            console.log(JSON.stringify(neodoc.run(`\
              usage: prog -h
              options:
                -?, -h, --help
            `, { argv: [flag] }
            )))
          });
          expect(result).to.deep.equal({
            code: 0
          , stderr: ''
          , stdout: `\
              usage: prog -h
              options:
                -?, -h, --help`
          });
        }
      });
    });

    describe('option.versionFlags', () => {
      it('should print help', () => {
        for (let flag of ['-v', '--version']) {
          const result = runFakeProc(() => {
            console.log(JSON.stringify(neodoc.run(`\
              usage: prog -v
              options:
                -v, --version
            `, { argv: [flag], version: '1.0.0' }
            )))
          });
          expect(result).to.deep.equal({
            code: 0
          , stderr: ''
          , stdout: '1.0.0'
          });
        }
      });

      describe('#73 - false positive check of special flags', () => {
        it('should not trigger on negative fallback', () => {
          for (let [ help, args, expected ] of [
            [ '', [], {} ]
          , [ '[-h]', [], {} ]
          , [ '[-xh]', [], {} ]
          , [ '[-xh]', ['-x'], {'-x': true} ]
          , [ '[-xh]', ['-h=false'], 'option takes no argument' ]
          ]) {
            const result = runFakeProc(() => {
              console.log(JSON.stringify(neodoc.run(`
                usage: prog ${help}
                options:
                  -h, --help
              `, { argv: args }
              )))
            });
            if (typeof expected === 'string') {
              expect(_.omit(result, 'stderr')).to.deep.equal({
                code: 1
              , stdout: ''
              });
              expect(result.stderr).to.contains(expected);
            } else {
              expect(result).to.deep.equal({
                code: 0
              , stderr: ''
              , stdout: JSON.stringify(expected)
              });
            }
          }
        });
      });
    });
  });

  describe('transform hooks', () => {
    describe('presolve hooks', () => {
      it('should get called once per run', () => {
        let i = 0, k = 0;
        const args = neodoc.run(`\
          usage: prog -a -b -c
        `, {
          dontExit: true
        , argv: ['-a', '-b', '-c']
        , transforms: {
            presolve: [
              spec => {
                i += 1;
                return spec;
              }
            , spec => {
                k += 1;
                return spec;
              }
            ]
          }
        });

        expect({ i, k }).to.deep.equal({ i: 1, k: 1 });
        expect(args).to.deep.equal({
          '-a': true
        , '-b': true
        , '-c': true
        });
      });

      it('should be able to change the spec', () => {
        const args = neodoc.run(`\
          usage: prog foo
        `, {
          dontExit: true
        , argv: []
        , transforms: {
            presolve: [
              spec => {
                spec.layouts = [];
                return spec;
              }
            ]
          }
        });
      });
    });

    describe('postsolve hooks', () => {
      it('should get called once per run', () => {
        let i = 0, k = 0;
        const args = neodoc.run(`\
          usage: prog -a -b -c
        `, {
          dontExit: true
        , argv: ['-a', '-b', '-c']
        , transforms: {
            presolve: [
              spec => {
                i += 1;
                return spec;
              }
            , spec => {
                k += 1;
                return spec;
              }
            ]
          }
        });

        expect({ i, k }).to.deep.equal({ i: 1, k: 1 });
        expect(args).to.deep.equal({
          '-a': true
        , '-b': true
        , '-c': true
        });
      });

      it('should be able to change the spec', () => {
        const args = neodoc.run(`\
          usage: prog foo
        `, {
          dontExit: true
        , argv: []
        , transforms: {
            presolve: [
              spec => {
                spec.layouts = [];
                return spec;
              }
            ]
          }
        });
      });
    });
  });

  describe('issues', () => {
    describe('#71 - remove left & right trimming', () => {
      it('should not trim the help text', () => {
        const result = runFakeProc(() => {
          neodoc.run(`\
            ${chalk.inverse(' THIMBLE ')} A scaffolding system that grows with you.
            ${chalk.blue('I. Usage:')}
              thimble <command> [<args>...]
              thimble -h | --help
              thimble --version
          `, { argv: [ '--help' ] });
        });
        expect(result).to.deep.equal({
          code: 0
        , stderr: ''
        , stdout: `\
            ${chalk.inverse(' THIMBLE ')} A scaffolding system that grows with you.
            ${chalk.blue('I. Usage:')}
              thimble <command> [<args>...]
              thimble -h | --help
              thimble --version`
        });
      });
    });
  });
});
