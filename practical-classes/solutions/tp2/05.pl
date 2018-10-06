prime(2).
prime(3).
prime(N) :-
    integer(N),
    N > 3,
    N mod 2 =\= 0,
    \+factor(N, 3). % Starts by 3 and checks every odd number.

factor(N, L) :- N mod L =:= 0.
factor(N, L) :- 
    L * L < N,
    L2 is L + 2,
    factor(N, L2).