/* 
	a) functor2(Term, F, Arity) checks if:
		- at the head of the list returned by univ (=..) is the functor name (F)
		- the remaining of that list are the args, therefore it's length must be equal to Arity
*/

functor2(Term, F, Arity):-
	length(ArgList, Arity),
	Term =.. [F|ArgList].

/*
	b) arg2(N, Term, Arg) checks if:
		- in the arguments list of Term, at the N position (1-index based) the argument is Arg
*/

%N in 1-index based
arg2(N, Term, Arg):-
	Term =.. [_|ArgList],
	element_at(N, Arg, ArgList).

%1-index base
element_at(1, Arg, [Arg|_]).
element_at(N, Arg, [H|T]):-
	Arg \= H,
	N1 is N - 1,
	element_at(N1, Arg, T).