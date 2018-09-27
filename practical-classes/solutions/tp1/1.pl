male('Aldo Burrows').
male('Lincoln Burrows').
male('LJ Burrows').
male('Michael Scofield').

female('Christina Rose Scofield').
female('Lisa Rix').
female('Sara Tancredi').
female('Ella Scofield').

%X is parent of Y
parent('Aldo Burrows', 'Lincoln Burrows').
parent('Lincoln Burrows','LJ Burrows').
parent('Lisa Rix','LJ Burrows').
parent('Christina Rose Scofield', 'Michael Scofield').
parent('Michael Scofield', 'Ella Scofield').
parent('Sara Tancredi', 'Ella Scofield').

%X is mother of Y
mother(X, Y):-
    parent(X, Y),
    female(X).

%X is father of Y
father(X, Y):-
    parent(X, Y),
    male(X).


/*
    a) mother(Y, 'Michael Scofield').

    father(X, 'Michael Scofield') is false due to lack of knowledge

    b) father('Aldo Burrows', X).

*/