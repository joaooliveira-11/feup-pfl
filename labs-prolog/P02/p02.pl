r(a, b).
r(a, d).
r(b, a).
r(a, c).

s(b, c).
s(b, d).
s(c, c).
s(d, e).

pairs(X, Y):- d(X), q(Y).
pairs(X, X):- u(X).
u(1).
d(2).
d(4).
q(4).
q(16).


a(a1, 1).
a(A2, 2).
a(a3, N).
b(1, b1).
b(2, B2).
b(N, b3).
c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).

% (4) Recursion

factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N -1,
    factorial(N1, Result1),
    Result is N * Result1.

sum_rec(0,0).
sum_rec(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_rec(N1, Sum1),
    Sum is Sum1 + N.

pow_rec(_, 0, 1).
pow_rec(X, Y, Result) :-
    Y > 0,
    Y1 is Y - 1,
    pow_rec(X, Y1, Result1),
    Result is X * Result1.

fibonacci(0, 0).
fibonacci(1,1).
fibonacci(N, Result) :-
    N > 0,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, Result1),
    fibonacci(N2, Result2),
    Result is Result1 + Result2.

collatz(1, 0).
collatz(N, Result) :-
    N > 0,
    0 is N mod 2,
    N1 is N // 2,
    collatz(N1, Result1),
    Result is Result1 + 1.
collatz(N, Result) :-
    N > 0,
    1 is N mod 2,
    N1 is (3*N)+1,
    collatz(N1, Result1),
    Result is Result1 + 1.


% (5) Tail Recursion

tail_factorial(N, Result) :-
    factorial(N, 1, Result).

factorial(0, Result, Result).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N -1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).


tail_sum(N, Result) :-
    sum_rec(N, 0 , Result).

sum_rec(0, Result, Result).
sum_rec(N, Acc, Result) :-
    N > 0,
    Acc1 is Acc + N,
    N1 is N - 1,
    sum_rec(N1, Acc1, Result).


tail_pow(N, P, Result) :-
    pow_rec(N, P, 1, Result).

pow_rec(_,0, Result, Result).
pow_rec(N, P, Acc, Result) :-
    P > 0,
    Acc1 is N * Acc,
    P1 is P - 1,
    pow_rec(N, P1, Acc1, Result).


tail_fibonacci(N, Result) :-
    fibonacci(N, 0, 1, Result).

fibonacci(0, Result, _, Result).
fibonacci(N, Acc1, Acc2, Result) :-
    N > 0,
    NextAcc1 is Acc2,
    NextAcc2 is Acc1 + Acc2,
    N1 is N -1,
    fibonacci(N1, NextAcc1, NextAcc2, Result). 


% (6)

