:- use_module(library(clpfd)).
:- use_module(library(lists)).

%p3
p3(L1,L2) :-
    length(L1,N),
    length(L2,N),
    pos(L1,L2,Is),
    all_distinct(Is),
    test(L2),
    labeling([],Is).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    element(I,L2,X),
    pos(Xs,L2,Is).


test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 #< X2 #/\ X2 #< X3) #\/ (X1 #> X2 #/\ X2 #> X3),
    test([X2,X3|Xs]).

%p4
build(Budget, NPacks, ObjectCosts, ObjectPacks, Objects, UsedPacks):-
	length(ObjectCosts, N),
	
	Objects = [O1, O2, O3],
	domain(Objects, 1, N),
	all_distinct(Objects),

	%ensure budget
	element(O1, ObjectCosts, C1),
	element(O2, ObjectCosts, C2),
	element(O3, ObjectCosts, C3),
	C1 + C2 + C3 #=< Budget,

	%number of packs
	UsedPacks in 1..NPacks,
	element(O1, ObjectPacks, N1),
	element(O2, ObjectPacks, N2),
	element(O3, ObjectPacks, N3),
	UsedPacks #= N1 + N2 + N3,
	labeling([maximize(UsedPacks)], Objects).