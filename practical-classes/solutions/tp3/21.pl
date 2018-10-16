slice(L, I1, I2, Slice):-
    I3 is I2+1,
    aux_slice(L, I1, I3, 1, Slice, []).

aux_slice(_, _, I2, I2, Acc, Acc).

aux_slice([H|T], I1, I2, I, Slice, Acc):-
    I >= I1, 
    I =< I2,
    NI is I+1, 
    append(Acc, [H], Acc1),
    aux_slice(T, I1, I2, NI, Slice, Acc1).

aux_slice([_|T], I1, I2, I, Slice, Acc):-
    NI is I+1,
    aux_slice(T, I1, I2, NI, Slice, Acc).
