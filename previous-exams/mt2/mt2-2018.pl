:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(between)).

%p1
prog1(N,M,L1,L2) :-
    length(L1,N),
    N1 is N-1, 
    length(L2,N1),
    findall(E,between(1,M,E),LE),
    fill(L1,LE,LE_),
    fill(L2,LE_,_),
    check(L1,L2).

fill([],LEf,LEf).
fill([X|Xs],LE,LEf) :-
    select(X,LE,LE_),
    fill(Xs,LE_,LEf).

check([_],[]).
check([A,B|R],[X|Xs]) :-
    A+B =:= X,
    check([B|R],Xs).

/*
    O predicado fill/3 leva como argumentos 2 listas L1, na qual só está definido o seu tamanho, e LE, na qual possuímos todos os 
    elementos de 1 a M, e retorna em L1 os N elementos selecionados e em LE_ os restantes elementos da lista LE.

    O predicado check/2 leva como argumentos 2 listas L1 e L2 e sucede se para cada par de elementos consecutivos em L1 a soma destes 
    for o proximo elemento de L2, pelo que L2 deverá ter menos um elemento que L1, algo já definido no predicado prog1.

    O predicado prog1/4 gera então duas listas, L1 e L2, em que L1 tem tamanho N e L2 tem tamanho N - 1.
    Cada elemento de cada uma das listas tem que estar obrigatoriamente dentro do intervalo [1, M]. 
    O preenchimento destas listas é feito pelo predicado fill, aceitando apenas valores que ocorrem dentro do intervalo.
    Os argumentos requiridos são M, e N que tem de ser M/2 arredondado para cima.
    
    A soma de cada par de elementos da lista L1 é igual ao elemento da lista L2, garantido pelo predicado check.
    Ou seja, se a solução for: 
        L1 = [1, 2, 4, 5] ; L2 = [3, 6, 9]
    
    o predicado check garante que: 1+2 = 3 ; 2+4 = 6 ; 4+5 = 9
*/

%p2 
/*  M^(2N−1)

    O predicado select/3 será efetuado M vezes, uma vez que os possíveis valores de X são todos os da lista de [1, M].
    Isto será feito para cada elemento das listas de tamanho N e N-1 (L1 e L2), logo temos M^N para L1 e M^N-1 para L2.
    Assim temos M^N + M^N-1 ou seja M^(2N-1).
*/

%p3
prog2(N,M,L1,L2) :-
    length(L1,N),
    N1 is N-1, 
    length(L2,N1),
    domain(L1,1,M),
    domain(L2,1,M),
    all_distinct(L1), /* Elementos diferentes, não são solução listas L1 com elementos repetidos */
    S #= N + N1,
    length(L3,S),
    append(L1,L2,L3),
    all_distinct(L3),
    removeSymmetries(L1), /* Remoção das simetrias ordenando os elementos da lista L1 */
    checkCLP(L1,L2),
    labeling([],L1).


checkCLP([_],[]).
checkCLP([A,B|R],[X|Xs]) :-
    A + B #= X,
    checkCLP([B|R],Xs).

removeSymmetries([_]).
removeSymmetries([X1, X2|Xs]):-
    X1 #=< X2,
    removeSymmetries([X2|Xs]).


%p4
% | ?- gym_pairs([95,78,67,84],[65,83,75,80],10,Pairs).
% no
% | ?- gym_pairs([95,78,67,84],[65,83,75,86],10,Pairs).
% Pairs = [1-4,2-3,3-1,4-2] ? ;
% no
% | ?- gym_pairs([95,78,67,84],[65,77,75,86],10,Pairs).
% Pairs = [1-4,2-2,3-1,4-3] ? ;
% Pairs = [1-4,2-3,3-1,4-2] ? ;
% no
gym_pairs(MenHeights, WomenHeights, Delta, Pairs):-
    length(MenHeights, NPairs),
    length(Men, NPairs),
    length(Women, NPairs),
    domain(Men, 1, NPairs),
    domain(Women, 1, NPairs),
    all_distinct(Men),
    all_distinct(Women),
    restrictHeights(Men, Women, Delta, MenHeights, WomenHeights),
    removeGymSymmetries(Men),
    append(Men, Women, Vars),
    labeling([], Vars),
    pairUp(Men, Women, Pairs, []).

restrictHeights([], [], _, _, _).
restrictHeights([M|Men], [W|Women], Delta, MenHeights, WomenHeights):-
    element(M, MenHeights, MH),
    element(W, WomenHeights, WH),
    MH #>= WH,
    Delta #>= MH - WH,
    restrictHeights(Men, Women, Delta, MenHeights, WomenHeights).

removeGymSymmetries([_]).
removeGymSymmetries([M1, M2|Men]):-
    M1 #=< M2,
    removeGymSymmetries([M2|Men]).

pairUp([], [], Pairs, Pairs).
pairUp([M|Men], [W|Women], Pairs, Acc):-
    append(Acc, [M-W], Acc2),
    pairUp(Men, Women, Pairs, Acc2).


%p5

% Uma escola de patinagem artística pretende ter um programa que obtenha, de forma
% automática, emparelhamentos de alunos para as suas aulas.
% Dadas as alturas dos homens e das mulheres presentes na aula, pretendem-se
% emparelhamentos em que a diferença de alturas entre o homem e a mulher seja inferior
% a um delta. O homem nunca poderá ser mais baixo do que a mulher.
% Por vezes não é possível emparelhar todas as pessoas presentes numa aula. Contudo, é
% útil saber que pares é possível formar, ficando as pessoas não emparelhadas a assistir à
% aula. Pode até acontecer que o número de homens e de mulheres na aula sejam
% diferentes, o que inviabiliza a constituição de pares para todas as pessoas.
% Construa um programa em PLR que permita obter o maior número possível de
% emparelhamentos. O predicado
% optimal_skating_pairs(+MenHeights,+WomenHeights,+Delta,-Pairs) recebe as alturas dos
% homens e das mulheres (listas não necessariamente com o mesmo tamanho) e a
% diferença máxima de alturas; devolve em Pairs o maior número possível de
% emparelhamentos de pessoas, identificadas pelo seu índice, que cumpram as restrições.
% Exemplos:
% | ?- optimal_skating_pairs([95,78,67,84],[65,83,75,80],10,Pairs).
% Pairs = [2-3,3-1,4-2]
% | ?- optimal_skating_pairs([95,78,67,84,65,90,77],[65,83,75,80],10,Pairs
% ).
% Pairs = [4-4,5-1,6-2,7-3]
% | ?- optimal_skating_pairs([65,83,75,80],[95,78,67,84,65,90,77],10,Pairs
% ).
% Pairs = [1-5,2-2,3-3,4-7]
% | ?- optimal_skating_pairs([95,78,67,84,65,90,77],[55,83,75,80],10,Pairs
% ).
% Pairs = [4-4,6-2,7-3]
% | ?- optimal_skating_pairs([55,83,75,80],[95,78,67,84,65,90,77],10,Pairs
% ).
% Pairs = [2-2,3-3,4-7]

optimal_skating_pairs(MenHeights, WomenHeights, Delta, Pairs):-
    length(MenHeights, NMen),
    length(WomenHeights, NWomen),
    minimum(Min, [NMen, NWomen]),
    NPairs in 1..Min,

    length(Men, NPairs),
    length(Women, NPairs),
    domain(Men, 1, NMen),
    domain(Women, 1, NWomen),
    all_distinct(Men),
    all_distinct(Women),

    restrictHeights(Men, Women, Delta, MenHeights, WomenHeights),
    removeGymSymmetries(Men),

    append(Men, Women, Vars),
    labeling([maximize(NPairs)], Vars),
    pairUp(Men, Women, Pairs, []).
