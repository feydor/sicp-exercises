function p() { return p(); }
function test(x, y) {
  return x === 0 ? 0 : y;
}

test(0, p());

/**
 * An applicative-order evaluation will continuously substitute
 * p() in test(0, p()) with the return statement / definition of p() until the call stack overflows.
 *
 * A normal-order evaluation will immediately evaluate the return statement of test(0, p()) by substituting the x and y with 0 and p(). It will return 0 immediately.
 *
 */

// node is an applicative-order evaluator.
