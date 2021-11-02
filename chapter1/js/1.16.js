// SICP JS Ex1.16
// Iterative exponentiation

function fast_expnt(b, n) {
  function expnt_itr(a, b, n) {
    return n == 0
           ? a
           : is_even(n)
           ? expnt_itr(a, square(b), n / 2)
           : expnt_itr(a * b, b, n - 1)
  }

  function is_even(n) { return n % 2 === 0; }
  function square(n) { return n * n; }

  return expnt_itr(1, b, n);
}

console.log(fast_expnt(2, 3));
