:-use_module(library(clpfd)).

cripto(ListaRes):-

  %variaveis
  Vars=[A,B,D,E,G,L,N,O,R,T],

  %dominio
  domain(Vars,0,9),

  %restriçoes
  all_distinct(Vars),
  D+G #= R,
  O+E #= O,
  N+R #= B,
  A+A #= E,
  L+L #= R,
  D+G #= T,

  %evitar simetrias

  %result
  labeling([],Vars),
  ListaRes = Vars.
