scalar_product(L1, L2, N):-
    length(L1, Length1),
    length(L2, Length2),
    Length1 == Length2,
    scalar_aux(L1, L2, 0, N).

scalar_aux([], [], Acc, Acc).
scalar_aux([H1|T1], [H2|T2], Acc, N):-
    Acc1 is Acc + H1 * H2,
    scalar_aux(T1, T2, Acc1, N).
