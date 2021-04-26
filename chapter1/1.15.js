// SICP JS Ex1.15

function abs(x) {
    return x >= 0 ? x : - x;
}

function cube(x) {
    return x * x * x;
}
function p(x) {
    return 3 * x - 4 * cube(x);
}
function sine(angle) {
    return ! (abs(angle) > 0.1)
           ? angle
           : p(sine(angle / 3));
}

sine(Math.PI / 2);

// a. for sine(12.15), function p is applied 5 times
// b. the order of growth is theta(log a), as each iteration's
// input is divided by 3.
