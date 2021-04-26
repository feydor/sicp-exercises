// SICP JS Ex1.17

function times(a, b) {
  return b == 0
         ? 0
         : a + times(a, b - 1);
}

function fast_times(a, b) {
  function double(n) { return 2 * n; }
  function halve(n) { return n / 2; }
  function is_even(n) { return n % 2 === 0; }

  return b === 0
         ? 0
         : is_even(b)
         ? fast_times(double(a), halve(b))
         : a + fast_times(a, b - 1);
}

console.log(fast_times(55, 40)); // 2200
