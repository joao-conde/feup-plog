:-use_module(library(clpfd)).

magicquad(ListaRes):-
  %variaveis
  Vars = [A,B,C,D,E,F,G,H,I],

  %dominio
  domain(Vars,1,9),

  %restriçoes
  all_distinct(Vars),
  A+B+C#=S,
  D+E+F#=S,
  G+H+I#=S,
  A+D+G#=S,
  B+E+H#=S,
  C+F+I#=S,
  A+E+I#=S,
  C+E+G#=S,

  %evitar simetrias
  A#<C,
  A#<G,
  A#<I,
  C#<G,

  %result
  labeling([],Vars),
  ListaRes = Vars.
