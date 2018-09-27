:-use_module(library(clpfd)).

ppp(ListaRes):-
  %variaveis
  Vars=[M,U,PPP],

  %dominio
  M in 1..9,
  U in 0..9,

  %restriçoes
  72 * PPP #= M * 1000 + 670 + U,

  %evitar simetrias

  %result
  labeling([],Vars),
  ListaRes = Vars.
