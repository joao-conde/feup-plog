% a)
delete_one(X,L1,L2):-
    append(La, [X|Lb], L1),
    append(La, Lb, L2).

% b)
%all list examined
delete_all(_, [], []).

%element at the head is X, don't add to new list
delete_all(X, [X|T], Y) :-
    delete_all(X, T, Y).

%element at the head is not X, add to new list
delete_all(X, [Z|T], [Z|Y]) :-
    X \= Z,
    delete_all(X, T, Y).

% c)
delete_all_list([], L1, L1).

delete_all_list([H|T], L1, L2):-
    delete_all(H, L1, L3),
    delete_all_list(T, L3, L2).
