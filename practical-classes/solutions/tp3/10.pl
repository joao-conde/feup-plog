% a)
is_sorted([_|[]]). %last element [X_| [] ]
is_sorted([H1, H2|T]):-
    H1 =< H2,
    is_sorted([H2|T]).

% b)
insert_sort(List, Sorted):-
    i_sort(List, [], Sorted).

i_sort([], Acc, Acc).
i_sort([H|T], Acc, Sorted):-
    insert(H, Acc, NAcc),
    i_sort(T, NAcc, Sorted).
   
insert(X, [Y|T], [Y|NT]):-
    X > Y,
    insert(X, T, NT).

insert(X, [Y|T], [X,Y|T]):-
    X =< Y.

insert(X,[],[X]).