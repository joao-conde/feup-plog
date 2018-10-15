is_permutation([], _).
is_permutation([H|T], L2):-
    [H|T] \= L2,
    member(H, L2),
    is_permutation(T, L2).