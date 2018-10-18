printInitialBoard :-
    initialBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

printMidBoard :-
    midgameBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

printEndBoard :-
    endgameBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

player1(white).
player2(black).

playerTurn(white, 'White').
playerTurn(black, 'Black').

lineNumbers(['19','18','17','16','15','14','13','12','11','10',' 9',' 8',' 7',' 6',' 5',' 4',' 3',' 2',' 1']).

/* Recursive function to print current board state */
printBoard([],[]) :-
    write('   |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|'), nl,
    write('   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S').

/*printBoard([],[]) :-
    write('  '), putCode(25,205), nl,
    write('   A  B  C  D  E  F  G  H ').*/

printBoard([Line|Board],[LineNumb|Remainder]) :-
    write('   |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|'), nl,
    %write('  '), putCode(25,205),  nl,
    write(LineNumb), write(' '),
    printLine(Line),
    write('|'), nl,
    printBoard(Board,Remainder).

/* Recursive function to print each board's line */
printLine([]).
printLine([Head|Tail]) :-
    translate(Head,T),
    write('|'),
    %put_code(186),
    write(T),
    printLine(Tail).

/* Get element at (row,col) */
getPiecePos(Column, Row, Message):-
	repeat,
    write(Message), nl,
	write('Column:'),
	getChar(Char),
	letter_to_int(Char, Column),
	write('Row:'),
	getInt(R),
	R =< 8,
	R >= 1,
	Row is R - 1.