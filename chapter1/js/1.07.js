// SICP Ex 1.7
// Better stop condition for approximating the square root
const TOLERANCE = 0.01;

function square(x) {
  return x * x;
}

function average(x, y) {
  return (x + y) / 2;
}

function improve(guess, x) {
  return average(guess, x / guess);
}

function is_good_enough(guess, x){
  return Math.abs(square(guess) - x) < TOLERANCE;
}

// current is_good_enough fails for very small numbers 
// because the tolerance is too permissive
// ex: sqrt(0.0001) yields 0.032308, the realt sqrt should be 0.01
// console.log(sqrt(0.0001));

// this version of is_good_enough looks at the difference between
// the current and next guess
function is_good_enough2(guess, x) {
  return Math.abs(guess - improve(guess, x)) < TOLERANCE;
}

function sqrt_itr(guess, x) {
  return is_good_enough2(guess, x)
         ? guess 
         : sqrt_itr(improve(guess, x), x)
}

function sqrt(x) {
  return sqrt_itr(1, x);
}

console.log(sqrt(0.0001));
console.log(sqrt(1000000000000)); // 12 0s
console.log(sqrt(10000000000000)); // 13 0s
