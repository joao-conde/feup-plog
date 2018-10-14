revert(Lista, InvLista):- rev(Lista, [], InvLista).

rev([], R, R).
rev([H|T], S, R):- rev(T, [H|S], R).