// SICP JS Ex 1.18

function fast_times_itr(a, b) {
  function double(n) { return 2 * n; }
  function halve(n) { return n / 2; }
  function is_even(n) { return n % 2 === 0; }

  // n holds the result
  function itr(n, a, b) {
    return b === 0
           ? n
           : is_even(b)
           ? itr(n, double(a), halve(b))
           : itr(n + a, a, b - 1);
  }

  return itr(0, a, b);
}

console.log(fast_times_itr(55, 40)); // 2200
