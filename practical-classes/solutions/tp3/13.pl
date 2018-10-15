% a) 
%TODO: why no work :(
% integers_up_to(0, []).
% integers_up_to(N, L):-
%     N > 0,
%     N1 is N-1,
%     integers_up_to(N1, [N|L]).

% a)
integers_up_to(N, L):-
    N > 0,
    build_list(N, 1, L).

build_list(0, _, []).
build_list(N, X, [X|L]):-
    N1 is N-1,
    X1 is X+1,
    build_list(N1, X1, L).

% b)
integers_between(N1, N2, L):-
    build_between_list(N1, N2, L).

build_between_list(N2, N2, [N2]).
build_between_list(N1, N2, [N1|L]):-
    N1 =< N2,
    N3 is N1+1,
    build_between_list(N3, N2, L).

% c)
sum_list(L, Sum):-
    sum_list_aux(L, 0, Sum).

sum_list_aux([], Acc, Acc).
sum_list_aux([H|T], Acc, Sum):-
    Acc1 is Acc+H,
    sum_list_aux(T, Acc1, Sum).

% d)
is_even(N):-
    N mod 2 =:= 0.

% e)
list_evens_up_to(N, L):-
    list_evens(N, [], L).

list_evens(0, Acc, Acc).
list_evens(N, Acc, L):-
    N > 0,
    is_even(N),
    N1 is N-1,
    list_evens(N1, [N|Acc], L).

list_evens(N, Acc, L):-
    N > 0,
    \+ is_even(N),
    N1 is N-1,
    list_evens(N1, Acc, L).

% f)
list_odds_up_to(N, L):-
    list_odds(N, [], L).

list_odds(0, Acc, Acc).
list_odds(N, Acc, L):-
    N > 0,
    N1 is N-1,
    \+ is_even(N),
    list_odds(N1, [N|Acc], L).

list_odds(N, Acc, L):-
    N > 0,
    N1 is N-1,
    is_even(N),
    list_odds(N1, Acc, L).