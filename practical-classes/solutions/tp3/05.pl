% a) 
my_member(X, [X|_]).
my_member(X, [_|Tail]):-
	my_member(X, Tail).

% b) 
my_member2(X,L):- append(_, [X|_], L).

% c) 
last_one(L,X):- append(_ ,[X], L).

% d)
nth_member(0, [M|_], M).
nth_member(N, [_|T], M):-
	N>0,
	N1 is N-1,
	nth_member(N1, T, M).