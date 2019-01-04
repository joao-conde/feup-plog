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
embrulha(Rolos, Presentes, RolosSelecionados):-
	length(Presentes, NPresentes),
	length(Rolos, NRolos),
	length(RolosSelecionados, NPresentes),
	domain(RolosSelecionados, 1, NRolos),
	for(NRolos, Rolos, RolosSelecionados, Presentes),
	ensurePresentsEnoughPaper(1, Presentes, RolosSelecionados, Rolos),
	labeling([], RolosSelecionados).


ensurePresentsEnoughPaper(_, _, [], _).
ensurePresentsEnoughPaper(Idx, Presentes, [Rolo|RolosSelecionados], Rolos):-
	element(Idx, Presentes, Papel),
	element(Rolo, Rolos, PapelRolo),
	Papel #=< PapelRolo,
	Idx2 #= Idx + 1,
	ensurePresentsEnoughPaper(Idx2, Presentes, RolosSelecionados, Rolos).


paperUsed(_, _, [], _, PapelUsado, PapelUsado).

paperUsed(Idx, Rolo, [R|RolosSelecionados], Presentes, PapelUsado, Acc):-
	element(Idx, Presentes, Val),
	Rolo #= R #=> Acc2 #= Acc + Val,
	Rolo #\= R #=> Acc2 #= Acc,
	Idx2 #= Idx + 1,
	paperUsed(Idx2, Rolo, RolosSelecionados, Presentes, PapelUsado, Acc2).

for(0, _, _, _).
for(Iter, Rolos, RolosSelecionados, Presentes):-
	paperUsed(1, Iter, RolosSelecionados, Presentes, PapelUsado, 0),
	element(Iter, Rolos, Total),
	PapelUsado #=< Total,
	Iter2 #= Iter-1,
	for(Iter2,  Rolos, RolosSelecionados, Presentes).