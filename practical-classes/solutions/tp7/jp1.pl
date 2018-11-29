% Considere um tabuleiro de xadrez com as coordenadas representadas por pares X/Y com X e Y entre
% 1 e 8.
% a) Defina salto_cavalo (Quad1, Quad2) que relaciona duas posições consecutivas dum cavalo de
% acordo com os movimentos possíveis deste. Assuma que Quad1 é sempre instanciado com as
% coordenadas de um quadrado.

:-use_module(library(clpfd)).

salto_cavalo(X0/Y0, X1/Y1):-
	X1 in 1..8,
	Y1 in 1..8,

	((X1 #= X0 + 2) #/\ (Y1 #= Y0 + 1)) #\/ ((X1 #= X0 + 1) #/\ (Y1 #= Y0 + 2)),
	labeling([], [X1, Y1]).
