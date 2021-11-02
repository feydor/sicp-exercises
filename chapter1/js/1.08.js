// SICP JS Ex 1.8
// Cube roots by Newton's Method
const ERROR_THRESHOLD = 0.01;

function square(x) {
  return x * x;
}

function cube(x) {
  return x * square(x);
}

function improve(guess, x) {
  return (x / square(guess) + 2 * guess) / 3;
}

function relative_error(reference, datum) {
  return Math.abs(reference - datum) / reference;
}

// using the difference between the guess and the next guess
// more accurate
function is_good_enough(guess, x) {
  return relative_error(guess, improve(guess, x)) < ERROR_THRESHOLD;
}

// usuing the difference between the guess and the final result
function is_good_enough2(guess, x) {
  return Math.abs(cube(guess) - x) < ERROR_THRESHOLD;
}

function cube_root_itr(guess, x) {
  return is_good_enough(guess, x)
         ? guess
         : cube_root_itr(improve(guess, x), x)
}

function cube_root(x) { return cube_root_itr(1, x) }

console.log(cube_root(27));            // 3
console.log(cube_root(0.0001));        // 0.0464 
console.log(cube_root(1000000000000)); // 12 0's, 1000
