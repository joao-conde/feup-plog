% IPLR 4 Problema dos Criptogramas
% O Problema dos CRIPTOGRAMAS consiste em atribuir dígitos decimais às letras, de modo a que a
% respectiva soma seja válida.

% Construa um programa CLP para resolver os seguintes criptogramas:
% puzzle(1,[D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]).
% puzzle(2,[0,C,R,O,S,S],[0,R,O,A,D,S],[D,A,N,G,E,R]).
% puzzle(3,[0,S,E,N,D],[0,M,O,R,E],[M,O,N,E,Y]).
% c) Construa um programa CLP para resolver criptogramas genéricos.


:-use_module(library(clpfd)).

% cripto(Word1, Word2):-
% 	length(L)