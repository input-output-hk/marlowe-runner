import jsonBigInt from "json-bigint";

export const patchersImpl = (function() {
  const { stringify, parse } = jsonBigInt({ useNativeBigInt: true });

  const parseImpl = function(resultHandlers, reviver, jsonStr) {
    try {
      return resultHandlers.success(JSON.parse(jsonStr, reviver));
    }
    catch (e) {
      return resultHandlers.failure(e.message);
    }
  };

  const stringifyImpl = function(replacer, space, value) {
    return JSON.stringify(value, replacer, space);
  };

  return {
    parseImpl: parseImpl,
    stringifyImpl: stringifyImpl,
    patchStringify: () => {
      // We need to patch the JSON.stringify in order for BigInt serialization to work.
      JSON.stringify = stringify;
    },
    patchParse: () => {
      JSON.parse = parse;
    }
  };
})();

export const isBigInt = (value) => typeof value === 'bigint';
