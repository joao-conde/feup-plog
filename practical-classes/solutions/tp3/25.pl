%pascal(Level, Line) -->Level of Pascal's triangle to unify with Line

%base cases
pascal(1, [1]).
pascal(2, [1,1]).

pascal(N, L):-
    Prev is N-1,
    pascal(Prev, L2),
    combined_pair_sum(L2, R),
    append([1|R], [1], L).


combined_pair_sum([X,Y], [Z]):- 
    Z is X + Y.

combined_pair_sum([X,Y|L], Z):- 
    H is X + Y,
    Z = [H|L2],
    combined_pair_sum([Y|L], L2).


%display Pascal triangle of N levels
display_pascal(N):-
    aux_display(N, 0).

aux_display(N, N).
aux_display(N, Cnt):-
    Cnt1 is Cnt+1,
    Level is Cnt+1,
    pascal(Level, Row),
    write(Row), nl,
    aux_display(N, Cnt1).