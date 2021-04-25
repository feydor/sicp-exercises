// SICP JS Ex1.10

// Ackermann's function
function A(x, y) {
  return y === 0
         ? 0
         : x === 0
         ? 2 * y
         : y === 1
         ? 2
         : A(x - 1, A(x, y - 1));
}

console.log(A(1, 10)); // 2 ^ 10 = 1024
console.log(A(2, 4)) // 2 ^ 2 ^ 2 ^ 2 = 65536
console.log(A(3, 3)); // 2 ^ 2 ^ 2 = 65536 

// f(n) = n * 2
function f(n) {
    return A(0, n);
}

// g(n) = 2 ^ n
function g(n) {
    return A(1, n);
}

// h(n) = 2 ^ 2 (^ 2) where the parenthisis is repeated n - 2 times
function h(n) {
    return A(2, n);
}

// k(n) = 5 * n^2
function k(n) {
   return 5 * n * n;
}
