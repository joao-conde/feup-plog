% a)
is_prime(N):-
    N > 1,
    prime_factors(N, 2).

prime_factors(Acc, Acc).
prime_factors(N, Acc):-
    N > Acc,
    N mod Acc =\= 0,
    Acc1 is Acc+1,
    prime_factors(N, Acc1).

% b)
list_primes(N, L):-
    list_primes_aux(N, [], L).

list_primes_aux(0, Acc, Acc).
list_primes_aux(N, Acc, L):-
    N > 0,
    N1 is N-1,
    is_prime(N),
    list_primes_aux(N1, [N|Acc], L).

list_primes_aux(N, Acc, L):-
    N > 0,
    N1 is N-1,
    \+ is_prime(N),
    list_primes_aux(N1, Acc, L).


