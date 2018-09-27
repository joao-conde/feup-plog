/* a)
membro(X, [X|_]).
membro(X, [_|Tail]) :-
  membro(X, Tail).
*/


% b) membro(X,L) :- append(_, [X|_], L).


% c) last(L,X) :- append(_ ,[X], L).

/* d)
nth_membro(0, [M|_], M).
nth_membro(N, [_|T], M) :-
    N>0,
    N1 is N-1,
*/
