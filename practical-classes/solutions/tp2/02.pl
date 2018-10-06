a(a1,1).
a(A,2).
a(a3,N).

b(1,b1).
b(2,B).
b(N,b3).

c(X,Y) :- a(X,N), b(N,Y).

d(X,Y) :- a(X,N), b(Y,N).
d(X,Y) :- a(N,X), b(N,Y).


/* 

    ?- a(X,2).

        yes

    ?- b(X,kalamazoo).

        X = 2 ?
        yes

    ?- c(X,b3).

        X = a1 ?
        yes


    ?- c(X,Y).

        X = a1,
        Y = b1 ? ;
        X = a1,
        Y = b3 ? ;
        yes

    ?- d(X,Y).

        X = a1,
        Y = 2 ? ;
        Y = 2 ? ;
        X = a3,
        Y = 1 ? ;
        X = a3,
        Y = 2 ? ;
        X = a3 ? ;
        X = 1,
        Y = b3 ? ;
        X = 2,
        Y = b1 ? ;
        X = 2 ? ;
        X = 2,
        Y = b3 ? ;
        Y = b3 ? ;
        no

*/