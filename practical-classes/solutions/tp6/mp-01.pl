%Test predicates
double(X,Y):- Y is 2*X.
f(X,Y):- Y is X*X.

map(L, Functor, MapList):-
	aux_map(L, Functor, MapList, []).

aux_map([], _, Acc, Acc).
aux_map([H|T], Functor, MapList, Acc):-
	Pred =.. [Functor, H, MapH],
	Pred,
	append(Acc, [MapH], Acc2),
	aux_map(T, Functor, MapList, Acc2).