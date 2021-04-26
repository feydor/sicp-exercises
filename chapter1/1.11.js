// SICP JS Ex1.11

// recursive process
function fn_r(n) {
  return n < 3
         ? n
         : fn_r(n - 1) + 2 * fn_r(n - 2) + 3 * fn_r(n - 3);
}

// iterative process
/*
// First draft version
function fn_i(n) {
  if (n >= 3) {
    let a = 2;
    let b = 1;
    let c = 0;
    for (let i = n; i >= 3; i--) {
      const _a = a + 2*b + 3*c;
      const _b = a;
      const _c = b;
      a = _a;
      b = _b;
      c = _c;
    } 
    return a;
  } else {
    return n;
  }
}
*/

// Final version
// initial state: 
// a = f(2) = 2
// b = f(1) = 1
// c = f(0) = 0
function fn_i(n) {
  function itr(a, b, c, n) {
    return n < 3 ? a : itr(a + 2*b + 3*c, a, b, n - 1);
  }
  return itr(2, 1, 0, n);
}

console.log('### recursive (tree) ###');
console.log(fn_r(4));
console.log(fn_r(22));
console.log('### iterative (tail) ###');
console.log(fn_i(4));
console.log(fn_i(22));
