% b) Defina o predicado elimina_duplicados(Lista1,Lista2) que elimina os duplicados em Lista1,
% resultando Lista2.

% a)
substitui(_, _, [], []).

substitui(X, Y, [X|T], [Y|L2]):-
    substitui(X, Y, T, L2).

substitui(X, Y, [H|T], [H|L2]):-
    X \= H, %safeguard
    substitui(X, Y, T, L2).  


% b)
%TODO: solve, bugged but close
elimina_duplicados([], []).

elimina_duplicados([H|T], [H|L2]):-
    \+ member(H, L2),
    elimina_duplicados(T, L2).

elimina_duplicados([H|T], L2):-
    member(H, L2),
    elimina_duplicados(T, L2).