
import bigInt from "big-integer";

export const rangeBigInt = function (start) {
  return function (end) {
    var step = start > end ? bigInt.minusOne : bigInt.one;
    var result = new Array(step * (end - start) + 1);
    var i = start, n = 0, j;
    while (!end.eq(i)) {
      j = bigInt(i.toString());
      result[n++] = i;
      i = i.add(step);
    }
    result[n] = i;
    return result;
  };
};
