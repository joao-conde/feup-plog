%%%%%%%%%%%%%%%%%%%%%
%   UTILS MODULE    %
%%%%%%%%%%%%%%%%%%%%%


/*  computeDistance(+[X1, Y1], +[X2, Y2], -Dis)
    
    Computes the squared Euclidean distance between [X1, Y1] and [X2, Y2].
*/
computeDistance([X1, Y1], [X2, Y2], Dis):-
    DiffX #= X2 - X1,
    DiffY #= Y2 - Y1,
    Dis #= DiffX * DiffX + DiffY * DiffY.


/*  printSolution(+Houses, +Connections)
    
    Prints the solution computed by the solver in an human-friendly way.
*/
printSolution(_, []).
printSolution(Houses, [I1, I2|Connections]):-
    nth1(I1, Houses, House1),
    nth1(I2, Houses, House2),
    computeDistance(House1, House2, Distance),
    write('Connection: '), write(House1), write(' <-> '), write(House2), write(' by a distance of '), write(Distance), nl,
    printSolution(Houses, Connections).


/*  randomLabeling(Var, _Rest, BB, BB1)

    Custom random labeling heuristic for value selection.
*/
randomLabeling(Var, _Rest, BB, BB1):-
    fd_set(Var, Set),
    randomSelector(Set, Value),
    (   
      first_bound(BB, BB1), Var #= Value;   
      later_bound(BB, BB1), Var #\= Value
    ).
  

/*  randomSelector(Set, BestValue)
    
    Randomly selects a variable from the Set.
*/
randomSelector(Set, BestValue):-
    fdset_to_list(Set, List),
    length(List, Len),
    random(0, Len, RandomIndex),
    nth0(RandomIndex, List, BestValue).


/*  puzzleFilePath(-FilePath)
    
    Gives the created puzzles file path.
*/
puzzleFilePath(FilePath):-
    current_directory(Dir),
    atom_concat(Dir, 'puzzles.pl', FilePath).


/*  savePuzzle(+Houses, +PuzzleName)
    
    Saves the puzzle Houses given with the name PuzzleName in the created puzzles file.
*/
savePuzzle(Houses, PuzzleName):-
    puzzleFilePath(PuzzlesPath),
    Term =.. [PuzzleName, Houses],    
    open(PuzzlesPath, append, Stream), 
    write(Stream, '\n'),
    write_term(Stream, Term, []), 
    write(Stream, '.\n'), 
    close(Stream),
    write(PuzzleName), write(' saved successfully at '), write(PuzzlesPath), nl, nl,
    reconsult(PuzzlesPath).