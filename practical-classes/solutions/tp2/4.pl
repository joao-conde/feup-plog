/* RECURSIVO
factorial(0,1).
factorial(N,F):-
  N > 0,
  N1 is N-1,
  factorial(N1,F1),
  F is F1*N.
*/

/* ITERATIVO */
factorial(0,1).
factorial(N,F) :- factorial(N,1,F).
factorial(1,F,F).
factorial(N,Acc,F) :- N>1, N1 is N-1,
  Acc1 is Acc*N,
  factorial(N1,Acc1,F).
