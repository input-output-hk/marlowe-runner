import yaml from 'js-yaml'
import { util } from 'js-yaml'


export const dumpImpl = yaml.dump;


export const bigIntSchema = (function() {
  // keep most of the original `int` options as is
  let options = Object.assign({}, yaml.types.int.options);

  options.construct = data => {
    let value = data, sign = 1n, ch;

    if (value.indexOf('_') !== -1) {
      value = value.replace(/_/g, '');
    }

    ch = value[0];

    if (ch === '-' || ch === '+') {
      if (ch === '-') sign = -1n;
      value = value.slice(1);
      ch = value[0];
    }

    return sign * BigInt(value);
  };

  options.predicate = object => {
    return Object.prototype.toString.call(object) === '[object BigInt]' ||
           yaml.types.int.options.predicate(object);
  };

  let BigIntType = new yaml.Type('tag:yaml.org,2002:int', options);

  return yaml.DEFAULT_SCHEMA.extend({ implicit: [ BigIntType ] });
})();
