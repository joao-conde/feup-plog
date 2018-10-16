drop_n(L1, N, L2):-
    Cnt is N+1,
    aux_drop_n(L1, N, Cnt, L2, []).

aux_drop_n([], _, _, Acc, Acc). 
aux_drop_n([_|T], N, Cnt, L2, Acc):-
    Cnt mod N =:= 0,
    Cnt1 is Cnt + 1,
    aux_drop_n(T, N, Cnt1, L2, Acc).

aux_drop_n([H|T], N, Cnt, L2, Acc):-
    Cnt mod N =\= 0,
    Cnt1 is Cnt + 1,
    append(Acc, [H], Acc1),
    aux_drop_n(T, N, Cnt1, L2, Acc1).