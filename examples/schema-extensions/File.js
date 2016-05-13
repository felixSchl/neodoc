const fs = require('fs');
const _ = require('lodash');
const BaseSchema = require('js-schema/lib/BaseSchema');
const File = module.exports = new BaseSchema.extend({
  validate: function(i) {
    i = _.isArray(i) ? i : [ i ];
    return _.every(i, fs.existsSync);
  }
, errors: function(i)  {
    const msg;
    const is = _.isArray(i) ? i : [ i ];
    const missing = _.filter(is, f => !fs.existsSync(f));
    if (missing.length > 0) {
      if (_.isArray(i)) {
        return ('Files do not exist:\n' +
                (_.map(missing, f => `  * ${f}`)).join('\n'));
      } else {
        return `File does not exist: ${i}`;
      }
    } else {
      return false;
    }
  }
, exists: () => new File().wrap()
, publicFunctions: ['exists']
});

File.schema = new File().wrap();
File.exists = File.schema.exists;
