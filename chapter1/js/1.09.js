// SICP JS Ex1.9
function inc(x) { return x + 1; }
function dec(x) { return x - 1; }

// this function essentially wraps b param with a inc params and resolves them when a is decremented to 0. This is a recursive function because it builds up a stack of deffered function calls.
function plus(a, b) {
    return a === 0 ? b : inc(plus(dec(a), b)); 
}

// this function adds param a to param b, one-by-one, until a is 0 and b 'contains' a. This is an iterative function because it has a fixed number of state variables (its params) and its iterations grow linearly with a.
function plus(a, b) {
    return a === 0 ? b : plus(dec(a), inc(b));
}

console.log(plus(4, 5));
