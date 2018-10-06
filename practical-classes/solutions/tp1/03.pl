livro('Os Maias').
livro('Harry Potter').
livro('The Casual Vacancy').

autor('Eca de Queiroz','Os Maias').
autor('J.K. Rowlling','Harry Potter').
autor('J.K. Rowlling','The Casual Vacancy').

nacionalidade('Eca de Queiroz','portugues').
nacionalidade('J.K. Rowlling','ingles').

tipo('Os Maias','romance').
tipo('Harry Potter', 'ficcao').
tipo('The Casual Vacancy', 'romance').

romancistaPortugues(X):-
    nacionalidade(X, 'portugues'),
    autor(X, _Y),
    tipo(_Y, 'romance').

/*
    a) autor(X,'Os Maias').

    b) romancistaPortugues(X).

    c) tipo(_X, 'ficcao'), autor(Y,_X), autor(Y,_Z), tipo(_Z, _T), _T \= 'ficcao'.
*/