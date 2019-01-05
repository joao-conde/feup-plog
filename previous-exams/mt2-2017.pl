:- use_module(library(lists)).

p1(L1,L2) :-
    gen(L1,L2),
    test(L2).

gen([],[]).
gen(L1,[X|L2]) :-
    select(X,L1,L3),
    gen(L3,L2).

test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 < X2, X2 < X3; X1 > X2, X2 > X3),
    test([X2,X3|Xs]).


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
:- use_module(library(clpfd)).

p2(L1,L2) :-
    length(L1,N),
    length(L2,N),
    pos(L1,L2,Is),
    all_distinct(Is),
    labeling([],Is),
    test(L2).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    nth1(I,L2,X),
    pos(Xs,L2,Is).

/*  
    a. As variáveis de domínio estão a ser instanciadas antes da fase de pesquisa e nem todas as
    restrições foram colocadas antes da fase da pesquisa.

    1 - são usados predicados que instanciam os valores a descobrir na fase de pesquisa, antes desta, como por
    exemplo nth1, e '<' ou '>' no predicado test.
    2 - a restrição imposta pelo predicado test/2 (quando corrigido para #> e #<) deve aparecer antes da fase de labeling (pesquisa).
*/


%p3 - correção do p2/2
p3(L1, L2) :-
    length(L1,N),
    length(L2,N),
    posPLR(L1,L2,Is),
    all_distinct(Is),
    testPLR(L2),
    labeling([],Is).

posPLR([],_,[]).
posPLR([X|Xs],L2,[I|Is]) :-
    element(I,L2,X),
    posPLR(Xs,L2,Is).

testPLR([_,_]).
testPLR([X1,X2,X3|Xs]) :-
    X1 #< X2 #=> X2 #< X3,
    X1 #> X2 #=> X2 #> X3,
    testPLR([X2,X3|Xs]).

%p4 

% | ?- sweet_recipes(60,30,[20,50,10,20,15],[6,4,12,20,6],Cookings,Eggs).
% Cookings = [1,3,5],
% Eggs = 24

% | ?- sweet_recipes(120,30,[20,50,10,20,15],[6,4,12,20,6],Cookings,Eggs).
% Cookings = [1,2,4],
% Eggs = 30

sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs):-
    length(RecipeTimes, NRecipes),

    Cookings = [C1, C2, C3],
    domain(Cookings, 1, NRecipes),
    all_distinct(Cookings),

    Eggs in 1..NEggs, 

    element(C1, RecipeTimes, T1),
    element(C2, RecipeTimes, T2),
    element(C3, RecipeTimes, T3),
    T1 + T2 + T3 #=< MaxTime,

    element(C1, RecipeEggs, E1),
    element(C2, RecipeEggs, E2),
    element(C3, RecipeEggs, E3),
    Eggs #= E1 + E2 + E3,

    labeling([maximize(Eggs)], Cookings).
    
