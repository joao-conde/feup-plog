:-use_module(library(lists)).
/*
    Implemente o predicado append(L1,L2,L) em que L é constituída pela concatenação das listas L1 e L2.
*/

append1([],L,L). 
append1([X|L1],L2,[X|L3]):- append1(L1,L2,L3).