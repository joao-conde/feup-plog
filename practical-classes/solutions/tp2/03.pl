exec(X,Y) :- p(X,Y).
exec(X,X) :- s(X).

p(X,Y) :- q(X), r(Y).
p(X,Y) :- s(X), r(Y).

q(a).
q(b).
r(c).
r(d).
s(e).


/*  
    call 'trace.' on console to debug and watch the program calls
    call 'notrace.' to turn off debug mode

    exec(X, Y) --> p(X, Y) --> q(X) --> q(X=a) --> r(Y) --> r(Y=c) --> X=a, Y=c ?

    with "no" or ";" (requesting more solutions) --> backtracking

    r(Y) --> r(Y=d) --> X=a, Y=d ; ?

    q(X) --> q(X=b) --> r(Y) --> r(Y=c) --> X=b, Y=c; ?
                        r(Y) --> r(Y=c) --> X=b, Y=d; ?

    p(X, Y) --> s(X) --> X=e --> r(Y) --> X=e, Y=c; ?
                                 r(Y) --> X=e, Y=d; ?

    exec(X, X) --> s(X) --> X=e, Y=e; ?

    no (fails)

*/
