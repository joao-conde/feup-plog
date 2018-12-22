computeDistance([X1, Y1], [X2, Y2], Dis):-
    DiffX #= X2 - X1,
    DiffY #= Y2 - Y1,
    Dis #= DiffX * DiffX + DiffY * DiffY.


printSolution(_, []).
printSolution(Houses, [I1, I2|Connections]):-
    nth1(I1, Houses, House1),
    nth1(I2, Houses, House2),
    computeDistance(House1, House2, Distance),
    write('Connection: '), write(House1), write(' <-> '), write(House2), write(' by a distance of '), write(Distance), nl,
    printSolution(Houses, Connections).


randomLabeling(Var, _Rest, BB, BB1) :-
    fd_set(Var, Set),
    randomSelector(Set, Value),
    (   
      first_bound(BB, BB1), Var #= Value;   
      later_bound(BB, BB1), Var #\= Value
    ).
  

randomSelector(Set, BestValue):-
    fdset_to_list(Set, List),
    length(List, Len),
    random(0, Len, RandomIndex),
    nth0(RandomIndex, List, BestValue).