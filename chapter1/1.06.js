// SICP JS 1.6
function conditional(predicate, then_clause, else_clause) {		    
    return predicate ? then_clause : else_clause;
}

function abs(x) {
    return x >= 0 ? x : - x;
}

function square(x) {
    return x * x;
}

function is_good_enough(guess, x) {
    return abs(square(guess) - x) < 0.001;
}

function average(x, y) {
    return (x + y) / 2;
}

function improve(guess, x) {
    return average(guess, x / guess);
}

function sqrt_iter(guess, x) {
    return conditional(is_good_enough(guess, x),
                       guess,
                       sqrt_iter(improve(guess, x),
                                 x));
}

sqrt_iter(3, 25);

/**
 * Since applicative-order evaluation is being used,
 * each expression of a return statement is substituted by its own
 * return statement. The third expression in the conditional call in sqrt_iter is substituted by the definition of sqrt_iter, ad infinitum. The call stack will overflow before the conditional function is ever evaluated.
 *
 *
 */
