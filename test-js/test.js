const neodoc = require('../');
const path = require('path');
const chalk = require('chalk');
const expect = require('chai').expect;

const EXAMPLES = path.resolve(__dirname, '..', 'examples');
const GIT_EXAMPLE = path.resolve(EXAMPLES, 'git');

const { runFakeProc, exec } = require('./support');

describe('neodoc', () => {
  describe('examples - git', () => {
    const git = (args) => JSON.parse(exec(GIT_EXAMPLE, args));

    it('git branch -u origin master', () => {
      expect(git('branch -u origin master'))
        .to.deep.equal({
          branch: true
        , '<branchname>': [ 'master' ]
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

  describe('specification parsing', () => {
    it ('should return the spec in regular JS', () => {
      const help = `
        Usage: foo <command> [options]
        Options:
          -f, --foo=BAR...
        `;

      expect(neodoc.parse(help)).to.deep.equal({
        program: 'foo'
      , help: help
      , shortHelp: 'Usage: foo <command> [options]'
      , specification:
          [
            [
              [
                {
                  "type": "Positional",
                  "value": {
                    "name": "<command>",
                    "repeatable": false
                  }
                },
                // each expanded [options] option gets it's own group i.o.t.
                // indiciate that it's optional. Repeatability is indiciated on
                // the group itself, rather than on the 'Option' node.
                {
                  "type": "Group",
                  "value": {
                    "optional": true,
                    "repeatable": true,
                    "branches":
                      [
                        [
                          { "type": "Option",
                            "value": {
                              "aliases": [ "-f", "--foo" ],
                              "repeatable": false,
                              "env": undefined,
                              "arg": {
                                  "name": "BAR",
                                  "default": undefined,
                                  "optional": false
                              }
                            }
                          }
                        ]
                      ]
                  }
                }
              ]
            ]
          ]
      });
    })
  });

  describe('specification loading', () => {
    it ('should parse argv using a JS spec', () => {
      const spec = {
        program: 'foo'
      , help: '...' /* does not matter */
      , shortHelp: 'Usage: foo <command> [options]'
      , specification:
          [
            [
              [
                {
                  "type": "Positional",
                  "value": {
                    "name": "<command>",
                    "repeatable": true
                  }
                },
                // each expanded [options] option gets it's own group i.o.t.
                // indiciate that it's optional. Repeatability is indiciated on
                // the group itself, rather than on the 'Option' node.
                {
                  "type": "Group",
                  "value": {
                    "optional": true,
                    "repeatable": false,
                    "branches":
                      [
                        [
                          { "type": "Option",
                            "value": {
                              "aliases": [ "-f", "--foo" ],
                              "repeatable": false,
                              "arg": {}
                            }
                          }
                        ]
                      ]
                  }
                }
              ]
            ]
          ]
      };

      expect(neodoc.run(spec, { argv: [ 'bar', '--foo', 'test' ] }))
        .to.deep.equal({
          '<command>': ['bar']
        , '--foo': 'test'
        , '-f': 'test'
        });
    })
  });

  describe('special arguments', () => {
    describe('option.helpFlags', () => {
      it('should print help', () => {
        for (let flag of ['-h', '--help', '-?']) {
          const result = runFakeProc(() => {
            console.log(JSON.stringify(neodoc.run(`
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
            console.log(JSON.stringify(neodoc.run(`
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
          const result = runFakeProc(() => {
            console.log(JSON.stringify(neodoc.run(`
              usage: prog [a -h]
              options:
                -h, --help
            `, { argv: ['a'] }
            )))
          });
          expect(result).to.deep.equal({
            code: 0
          , stderr: ''
          , stdout: JSON.stringify({ 'a': true })
          });
        });
      });
    });
  });

  describe('issues', () => {
    describe('#71 - remove left & right trimming', () => {
      it('should not trim the help text', () => {
        const result = runFakeProc(() => {
          neodoc.run(`
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
