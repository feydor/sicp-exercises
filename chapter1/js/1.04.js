/*function a_plus_abs_b evaluates plus(a, b) when b is positive or zero and minus(a, b) otherwise. */
function plus(a, b) { return a + b; }
function minus(a, b) { return a - b; }
function a_plus_abs_b(a, b) {
    return (b >= 0 ? plus : minus)(a, b);
}
