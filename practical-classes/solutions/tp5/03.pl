dados(um).
dados(dois).
dados(tres).

/*

    a) Qual o resultado da seguinte pergunta?
    cut_teste_a(X) :-
        dados(X).
    cut_teste_a('ultima_clausula').
    ?- cut_teste_a(X), write(X), nl, fail.
    
    b) Qual o resultado do seguinte programa com um Cut no final da primeira clausula?
    cut_teste_b(X):-
    dados(X), !.
    cut_teste_b('ultima_clausula').
    ?- cut_teste_b(X), write(X), nl, fail.
    c) Qual o resultado do seguinte programa com um Cut no meio dos dois objectivos?
    cut_teste_c(X,Y) :-
    dados(X),
    !,
    dados(Y).
    cut_teste_c('ultima_clausula').
    ?- cut_teste_c(X,Y), write(X-Y), nl, fail.

*/