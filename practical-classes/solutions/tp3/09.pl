% b) Defina o predicado rmv_dups(Lista1,Lista2) que elimina os duplicados em Lista1,
% resultando Lista2.

% a)
replace(_, _, [], []).

replace(X, Y, [X|T], [Y|L2]):-
    replace(X, Y, T, L2).

replace(X, Y, [H|T], [H|L2]):-
    X \= H, %safeguard
    replace(X, Y, T, L2).  


% b)
%TODO: solve, bugged but close
rmv_dups([], []).

rmv_dups([H|T], [H|L2]):-
    \+ member(H, L2),
    rmv_dups(T, L2).

rmv_dups([H|T], L2):-
    member(H, L2),
    rmv_dups(T, L2).