print_initial_board :-
    initial_board(X),  
    print_board(X).

print_mid_board :-
    midgame_board(X),
    print_board(X).

print_end_board :-
    endgame_board(X),
    print_board(X).

player1(white).
player2(black).

player_turn(white, 'White').
player_turn(black, 'Black').

line_numbers(['19','18','17','16','15','14','13','12','11','10',' 9',' 8',' 7',' 6',' 5',' 4',' 3',' 2',' 1']).

print_separator :-
    write('  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |'), nl.

print_hline :-
    write('  *-------------------------------------------------------------------------------*'), nl.

print_board(Board) :-
    line_numbers(LineNumbers),
    print_hline,
    print_board_aux(Board,LineNumbers),
    print_separator,
    print_hline,
    write('      A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S').

/* Recursive function to print current board state */
print_board_aux([],[]).    

print_board_aux([Line|Board],[LineNumb|Remainder]) :-
    print_separator,
    write(LineNumb), write('|-'),
    print_line(Line),
    write('--|'), nl,
    print_board_aux(Board,Remainder).

/* Recursive function to print each board's line */
print_line([]).
print_line([Head|Tail]) :-
    translate(Head,T),
    write('-'),
    write(T),
    print_line(Tail).

/* Get element at (row,col) */
get_piece_pos(Column, Row, Message):-
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

translate(0, '-+-').
translate(1, '-B-').
translate(2, '-W-').
% translate(1, Char):- %Black Pieces
%     char_code(Char, 8413),
%     write('-'), 
%     write(Char), 
%     write('-').

% translate(2, Char):- %White Pieces
%     char_code(Char, 8413),
%     write('-'), 
%     write(Char), 
%     write('-').