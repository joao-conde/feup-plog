flatten_list([], []).

flatten_list(X, [X]):-
    atomic(X).

flatten_list([H|T], L):-
    flatten_list(H, L1),
    flatten_list(T, L2),
    append(L1, L2, L).