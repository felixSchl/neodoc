const neodoc = require('../');
const path = require('path');
const $ = require('shelljs');
const expect = require('chai').expect;

const EXAMPLES = path.resolve(__dirname, '..', 'examples');
const GIT_EXAMPLE = path.resolve(EXAMPLES, 'git');

describe('neodoc', () => {
  describe('examples - git', () => {
    const git = (args) => {
      const p = $.exec(`node "${GIT_EXAMPLE}" ${args}`, { silent: true });
      if (p.code === 0) {
        return JSON.parse(p.stdout);
      } else {
        throw new Error(p.stdout);
      }
    }

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
      expect(neodoc.parse(`
        Usage: foo <command> [options]
        Options:
          -f, --foo=BAR...
        `
      )).to.deep.equal({
        shortHelp: 'Usage: foo <command> [options]\n'
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
                              "flag": "f",
                              "name": "foo",
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
    it ('should return the spec in regular JS', () => {
      const spec = {
        shortHelp: 'Usage: foo <command> [options]\n'
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
                              "flag": "f",
                              "name": "foo",
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
});
