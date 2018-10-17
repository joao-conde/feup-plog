:-use_module(library(random)).
% 'random/3' is deprecated but 'random_between/3' not found, possibly not in this SICStus Release (4.4.1)

% a)
rnd_select_n(L1, N, L2):-
    length(L1, Len1),
    Len2 is Len1-1,
    generate_rand_list(N, 0, Len2, RandList),
    aux_rnd_select_n(L1, RandList, Len1, 0, L2, []).

aux_rnd_select_n(_, _, Len, Len, Acc, Acc).
aux_rnd_select_n([H|T], RandList, Len, Cnt, L2, Acc):-
    member(Cnt, RandList),
    Cnt1 is Cnt+1,
    aux_rnd_select_n(T, RandList, Len, Cnt1, L2, [H|Acc]).


aux_rnd_select_n([_|T], RandList, Len, Cnt, L2, Acc):-
    \+ member(Cnt, RandList),
    Cnt1 is Cnt+1,
    aux_rnd_select_n(T, RandList, Len, Cnt1, L2, Acc).


generate_rand_list(N, LbClosed, UbOpen, L):-
    UbClosed is UbOpen + 1,
    aux_generate_rand_list(N, 0, LbClosed, UbClosed, L, []).

aux_generate_rand_list(N, N, _, _, Acc, Acc).
aux_generate_rand_list(N, Cnt, Lb, Ub, L, Acc):-
    Cnt < N,
    random(Lb, Ub, Rand),
    aux_add_rand(Rand, N, Cnt, Lb, Ub, L, Acc).


aux_add_rand(Rand, N, Cnt, Lb, Ub, L, Acc):-
    member(Rand, Acc),
    aux_generate_rand_list(N, Cnt, Lb, Ub, L, Acc).

aux_add_rand(Rand, N, Cnt, Lb, Ub, L, Acc):-
    \+ member(Rand, Acc),
    Cnt1 is Cnt+1,
    aux_generate_rand_list(N, Cnt1, Lb, Ub, L, [Rand|Acc]).


% b)
rnd_select(N, M, L):-
    M1 is M+1,
    generate_rand_list(N, 1, M1, L).

% c)
rnd_permutation(L1, L2):-
    aux_rnd_permutation(L1, L2, []).

aux_rnd_permutation([], Acc, Acc).
aux_rnd_permutation(L1, L2, Acc):-
    length(L1, Len),
    random(0, Len, Rand),
    get_el(L1, Rand, X, L3),
    aux_rnd_permutation(L3, L2, [X|Acc]).


get_el(L1, N, X, L2):-
    aux_get_el(L1, N, 0, X, L2, []).

aux_get_el([], _, _, _, Acc, Acc).

aux_get_el([H|T], Cnt, Cnt, H, L2, Acc):-
    Cnt1 is Cnt+1,
    aux_get_el(T, Cnt, Cnt1, H, L2, Acc).

aux_get_el([H|T], N, Cnt, X, L2, Acc):-
    Cnt1 is Cnt+1,
    aux_get_el(T, N, Cnt1, X, L2, [H|Acc]).
