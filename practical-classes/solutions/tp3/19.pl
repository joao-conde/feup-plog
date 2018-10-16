% a)
runlength(L1, L2):-
    aux_runlength(L1, L2, []).

aux_runlength([], Acc, Acc).

aux_runlength([H|T], L, Acc):-
    head_occurrences(H, [H|T], Cnt),
    discard_first([H|T], NL, Cnt),
    append(Acc, [[Cnt,H]], Acc1),
    aux_runlength(NL, L, Acc1).

%======================================%
%     Discards Cnt head occurrences    %
discard_first(L1, L2, Cnt):-
    aux_discard(L1, L2, Cnt, 0).

aux_discard(L, L, Cnt, Cnt). 
aux_discard([_|T], L2, Cnt, N):-
    N1 is N+1,
    aux_discard(T, L2, Cnt, N1).


%======================================%
%  Counts head occurrences of an item  %
head_occurrences(X, L, Cnt):-
    aux_count(X, L, Cnt, 0).

aux_count(_, [], AuxCnt, AuxCnt).
aux_count(X, [H|_], AuxCnt, AuxCnt):-
    X \= H.

aux_count(X, [X|T], Cnt, AuxCnt):-
    AuxCnt1 is AuxCnt + 1,
    aux_count(X, T, Cnt, AuxCnt1).

%======================================%


% b) 
modified_runlength(L1, L2):-
    aux_modified_runlength(L1, L2, []).

aux_modified_runlength([], Acc, Acc).


aux_modified_runlength([H|T], L, Acc):-
    head_occurrences(H, [H|T], Cnt),
    discard_first([H|T], NL, Cnt),
    Cnt > 1,
    append(Acc, [[Cnt,H]], Acc1),
    aux_modified_runlength(NL, L, Acc1).

aux_modified_runlength([H|T], L, Acc):-
    head_occurrences(H, [H|T], Cnt),
    discard_first([H|T], NL, Cnt),
    Cnt == 1,
    append(Acc, [H], Acc1),
    aux_modified_runlength(NL, L, Acc1).

%c)
%======================================%
%        Decompress list length        %
decompress(L1, L2):-
    aux_decompress(L1, L2, []).

aux_decompress([], Acc, Acc).
aux_decompress([[Cnt|X]|T], L, Acc):-
    build_list(X, Cnt, Prefix),
    extend(Prefix, Acc, Acc1),
    aux_decompress(T, L, Acc1).


%======================================%
%     Extends L2 with elements of L1   %
extend(L1, L2, L3):-
    aux_extend(L1, L2, L3, L2).

aux_extend([], _, Acc, Acc).
aux_extend([H|T], L2, L3, Acc):-
    append(Acc, [H], Acc1),
    aux_extend(T, L2, L3, Acc1).

%======================================%
%     Builds list of size N with X     %
build_list(X, N, L):-
    aux_build_list(X, N, 0, L, []).

aux_build_list(_, N, N, Acc, Acc).
aux_build_list(X, N, Cnt, L, Acc):-
    Cnt =< N,
    append(X, Acc, Acc1),
    Cnt1 is Cnt+1,
    aux_build_list(X, N, Cnt1, L, Acc1).
%======================================%


%======================================%
%    Modified decompress list length   %
modified_decompress(L1, L2):-
    aux_modified_decompress(L1, L2, []).

aux_modified_decompress([], Acc, Acc).


aux_modified_decompress([], Acc, Acc).

aux_modified_decompress([[Cnt|X]|T], L, Acc):-
    build_list(X, Cnt, Prefix),
    extend(Prefix, Acc, Acc1),
    aux_modified_decompress(T, L, Acc1).

aux_modified_decompress([X|T], L, Acc):-
    extend([X], Acc, Acc1),
    aux_modified_decompress(T, L, Acc1).
%======================================%
