function square(a) {
  return a * a;
}

function sum_of_squares(a, b) {
  return square(a) + square(b);
}

/* returns the larger of the two inputs,
 * if equal returns a
 */
function max(a, b) {
  return a > b ? a : b;
}

/* returns the sum of squares of the two larger inputs */
function greatest_sum_of_squares(a, b, c) {
  return a > b || a > c
         ? sum_of_squares(a, max(b, c))
         : sum_of_squares(c, b);
}

/* works regardless of order, 6 permutations for any three numbers */
console.assert(greatest_sum_of_squares(1, 2, 3) === 13);
console.assert(greatest_sum_of_squares(1, 3, 2) === 13);
console.assert(greatest_sum_of_squares(2, 1, 3) === 13);
console.assert(greatest_sum_of_squares(2, 3, 1) === 13);
console.assert(greatest_sum_of_squares(3, 1, 2) === 13);
console.assert(greatest_sum_of_squares(3, 2, 1) === 13);

console.assert(greatest_sum_of_squares(2, 2, 2) === 8);
