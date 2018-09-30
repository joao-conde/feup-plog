bird('Tweety').
fish('Goldie').
worm('Molie').
cat('Silvester').

%X and Y are friends
friends('me', 'Silvester').


%X eats Y
eats(X, Y):- bird(X), worm(Y).
eats(X, Y):- cat(X), fish(Y).
eats(X, Y):- cat(X), bird(Y).

%X likes Y
likes(X, Y):- friends(X, Y).


/* 

    a) eats('Silvester', Y).

    b) the answer is reasonable if we differenciate between 'liking' and 'eating' 

*/