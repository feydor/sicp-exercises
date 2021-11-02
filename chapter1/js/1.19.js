// SICP JS Ex1.19
function fib(n) {
  return fib_iter(1, 0, 0, 1, n); 
}

function square(x) { return x * x; }
function double(x) { return x + x; }
function is_even(x) { return x % 2 == 0; }

function fib_iter(a, b, p, q, count) {
  return count === 0 
         ? b 
         : is_even(count) 
         ? fib_iter(a,
                    b,
                    square(q) + square(p), // compute p' 
                    square(q) + double(q) * p, // compute q'
                    count / 2) 
         : fib_iter(b * q + a * q + a * p,
                    b * p + a * q,
                    p,
                    q,
                    count - 1); 
}

// T(T(a,b)) = a: a(q^2 + p^2) + a(q^2 + 2pq) + b(q^2 + 2pq);
//             b: a(q^2 + 2qp) + b(q^2 + p^2)
// and a: b(q') + a(q') + a(p'), b: b(p') + a(q')
// so q' = q^2 + 2qp and p' = q^2 + p^2
console.log(fib(10)); // 55
