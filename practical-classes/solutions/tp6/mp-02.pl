/*
	Test with:

	| ?- divide([1,2,3,4,5,6], is_pair, L).
		  L = [2,4,6,1,3,5] ? 
		  yes
*/

%Test predicate
is_pair(X):-
	X mod 2 =:= 0.

divide(L, Pred, L2):-
	aux_divide(L, Pred, True, False, [], []),
	append(True, False, L2).

aux_divide([], _, AccTrue, AccFalse, AccTrue, AccFalse).
aux_divide([H|T], Pred, True, False, AccTrue, AccFalse):-
	Exec =.. [Pred, H],
	Exec,
	append(AccTrue, [H], Acc2True),
	aux_divide(T, Pred, True, False, Acc2True, AccFalse).

aux_divide([H|T], Pred, True, False, AccTrue, AccFalse):-
	Exec =.. [Pred, H],
	\+ Exec,
	append(AccFalse, [H], Acc2False),
	aux_divide(T, Pred, True, False, AccTrue, Acc2False).
