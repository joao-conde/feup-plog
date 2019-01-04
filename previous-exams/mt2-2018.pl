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
    O predicado fill/3 leva como argumentos 2 listas L1 e LE e retorna em LEf a diferença entre as listas LE e L1, ou seja, um lista LEf
    com todos os elementos de LE que nao ocorrem em L1.

    O predicado check/2 leva como argumentos 2 listas L1 e L2 e sucede se para cada par de elementos consecutivos em L1 a soma destes for o proximo
    elemento de L2, pelo que L2 deverá ter menos um elemento que L1.

    O predicado prog1/4 gera então duas listas, L1 e L2, em que L1 tem tamanho N e L2 tem tamanho N - 1.
    Cada elemento de cada uma das listas tem que estar obrigatoriamente dentro do intervalo [1, M]. 
    O preenchimento destas listas é feito pelo predicado fill, aceitando apenas valores que ocorrem dentro do intervalo.
    
    A soma de cada par de elementos da lista L1 é igual ao elemento da lista L2, garantido pelo predicado check.
    Ou seja, se a solução for: 
        L1 = [1, 2, 4, 5] ; L2 = [3, 6, 9]
    
    o predicado check garante que: 1+2 = 3 ; 2+4 = 6 ; 4+5 = 9
*/

%p2 
/*  M^(2N−1)

    O predicado select/3 será efetuado M vezes, uma vez que os possíveis valores de X são todos os da lista de [1, M].
    Isto será feito para cada elemento das listas de tamanho N, logo temos M^N.
    Como é feito duas vezes temos M^(2N). 
    A opção mais semelhante seria a M^(2N−1).
*/

%p3
prog2(N,M,L1,L2) :-
    length(L1,N),
    N1 is N-1, 
    length(L2,N1),
    domain(L1,1,M),
    domain(L2,1,M),
    all_distinct(L1), /* Elementos diferentes, não são solução listas L1 com elementos repetidos */
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