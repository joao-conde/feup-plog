:- use_module(library(clpfd)).
:- use_module(library(lists)).

%p1
/*
    O predicado gen/2 sucede se as duas listas tiverem os mesmos elementos, por qualquer ordem.
    Assim, dadas L1 e L2, se L1 for dada como instanciada e L2 não, L2 será instanciada resultando numa lista
    com todos os elementos de L1. Por backtracking podem ser geradas todas as permutações de elementos
    da lista L1.

    O predicado test/2 sucede caso a lista esteja ordenada, quer por ordem crescente quer por ordem decrescente.

    Assim, o predicado p1/2 sucede se a lista L2 for constituida pelos elementos da lista L1 mas ordenada, em ordem
    crescente ou decrescente. Caso L1 seja instanciada e L2 não, em L2 será devolvida uma lista ordenada de todos os elementos
    de L1. É possível por backtracking pedir a solução decrescente ou crescente.

    Quanto a eficiência, o predicado test/2 é linear, ou seja, O(n), sendo n o tamanho da lista L2 e L1 (mesmo tamanho).
    No pior dos casos, a lista gerada por gen/2 só é uma solução após gerar todas as permutações dos elementos da lista L1,
    pelo que para gerar estas permutações temos O(n!).
    Para cada uma destas gerações teremos então a verificação feita em tempo linear, pelo que a complexidade total de p1/2
    é de O(n! * n).
*/

%p2
/*
    1 - são usados predicados que instanciam os valores a descobrir na fase de pesquisa, antes desta, como por
    exemplo nth1, e '<' ou '>' no predicado test.
    2 - a restrição imposta pelo predicado test/2 (quando corrigido para #> e #<) deve aparecer antes da fase de labeling (pesquisa).
*/

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