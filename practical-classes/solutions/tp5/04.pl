/*
    a)  If X is equal to Y, the first two predicates will both fail.
        If X = 3 and Y = 3 but Z = 2 (for example), the last clause will unify Z with 
        the max value which is false as shown above.

    b)  Correction below: change '>' for '>='.
        This way if X=Y and Z is smaller, max is X or Y.
*/
max(X, Y, Z, X):- 
    X>=Y, 
    X>=Z, 
    !.

max(X, Y, Z, Y):- 
    Y>=X, 
    Y>=Z, 
    !.

max(_, _, Z, Z).