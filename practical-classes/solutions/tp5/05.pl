% Defina o predicado unificavel(L1, Termo, L2) em que L2 é uma lista com todos os elementos de L1
% que são unificáveis com Termo. Os elementos de L2 não são no entanto unificados com Termo.
% Exemplo:
% ?- unificavel([X,b,t(Y)],t(a),L).
% L=[X,t(Y)]

unifiable(L1, Term, L2):-
    unifiable_aux(L1, Term, L2, []).


unifiable_aux([], _, Acc, Acc).
unifiable_aux([H|T], Term, L2, Acc):-
    H = Term,
    unifiable_aux(T, Term, L2, [H|Acc]).

unifiable_aux([H|T], Term, L2, Acc):-
    H \= Term,
    unifiable_aux(T, Term, L2, Acc).
