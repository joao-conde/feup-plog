% Rotate LEFT %
rotate(L1, N, L2):-
    N >= 0,
    aux_rotate_left(N, 0, L2, L1).

aux_rotate_left(N, N, Acc, Acc).
aux_rotate_left(N, Cnt, L2, [H2|T2]):-
    Cnt =< N,
    Cnt1 is Cnt+1,
    append(T2, [H2], Acc1),
    aux_rotate_left(N, Cnt1, L2, Acc1).

% Rotate RIGHT %
rotate(L1, N, L2):-
    N < 0,
    N1 is 0 - N,
    revert(L1, L3),
    aux_rotate_left(N1, 0, RevL2, L3),
    revert(RevL2, L2).




revert(L1, L2):-
    rev(L1, L2, []).

rev([], Acc, Acc).
rev([H|T], L1, Acc):-
    rev(T, L1, [H|Acc]).


