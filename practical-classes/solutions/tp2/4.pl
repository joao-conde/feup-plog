/* FACTORIAL RECURSIVO */
factorial(0,1).
factorial(N,F):-
  N > 0,
  N1 is N-1,
  factorial(N1,F1),
  F is F1*N.

/* FACTORIAL ITERATIVO */
factorialit(0,1).
factorialit(N,F) :- factorialit(N,1,F).
factorialit(1,F,F).
factorialit(N,Acc,F) :- 
  N>1, N1 is N-1,
  Acc1 is Acc*N,
  factorialit(N1,Acc1,F).

/* FIBONACCI */
fibonacci(0,1). 
fibonacci(1,1). 
fibonacci(N,F):-   
  N > 1, N1 is N - 1, 
  fibonacci(N1,F1), N2 is N - 2, 
  fibonacci(N2,F2), 
  F is F1 + F2.