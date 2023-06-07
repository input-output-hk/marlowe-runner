import jsonBigInt from "json-bigint";

export const patchers = (function() {
  const { stringify, parse } = jsonBigInt({ useNativeBigInt: true });

  return {
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
