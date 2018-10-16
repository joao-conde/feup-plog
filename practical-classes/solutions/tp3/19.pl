% a)
runlength(L1, L2):-
    aux_runlength(L1, L2, []).

aux_runlength([], Acc, Acc).

aux_runlength([H|T], L, Acc):-
    head_occurrences(H, [H|T], Cnt),
    discard_first([H|T], NL, Cnt),
    append(Acc, [[Cnt|H]], Acc1),
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
    append(Acc, [[Cnt|H]], Acc1),
    aux_modified_runlength(NL, L, Acc1).

aux_modified_runlength([H|T], L, Acc):-
    head_occurrences(H, [H|T], Cnt),
    discard_first([H|T], NL, Cnt),
    Cnt == 1,
    append(Acc, [H], Acc1),
    aux_modified_runlength(NL, L, Acc1).

% c) Construa as versões de descompressão de listas, codificadas em run-lenght nas duas alíneas
% anteriores.
decompress(L1, L2).