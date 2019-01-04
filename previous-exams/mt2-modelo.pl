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


%p5
embrulha(Rolos, Presentes, RolosSelecionados) :-
	length(Presentes, NPresentes),
	length(Rolos, NRolos),
	length(RolosSelecionados, NPresentes),
	domain(RolosSelecionados, 1, NRolos),
	machines(Rolos, 1, Machines), % machines -> paper tubes
	tasks(Presentes, RolosSelecionados, Tasks), % tasks -> gifts
	cumulatives(Tasks, Machines, [bound(upper)]),
	labeling([], RolosSelecionados).

machines([], _, []).
machines([H|T], Index, Machines) :-
	NextIndex is Index + 1,
	machines(T, NextIndex, TempMachines),
	% A machine needs an ID and an upper limit of resources at a given instant
	append(TempMachines, [machine(Index, H)], Machines).

tasks([], [], []).
tasks([HP|TP], [HS|TS], Tasks) :-
	tasks(TP, TS, TempTasks),
	% A task needs a start time, duration and end time, resources consumed and the machine it is done on
	append(TempTasks, [task(0, 1, 1, HP, HS)], Tasks).