request_enter:-
	write('Press <Enter> to continue.'), nl,
	wait_for_enter, !.

wait_for_enter:-
	get_char(_).

%predicate to convert letters from rows or columns to an integer
letter_to_int(Letter, Num) :-
    Letter @>= 'A',
    Letter @=< 'Z',
    char_code('A', AsciiA),
    char_code(Letter, AsciiL),
    Num is AsciiL - AsciiA.

letter_to_int(Letter, Num) :-
    Letter @>= 'a',
    Letter @=< 'z',
    char_code('a', AsciiA),
    char_code(Letter, AsciiL),
    Num is AsciiL - AsciiA.

%clears console by printing multiple newlines 
clear_console:- clear_console(50), !.
clear_console(0).
clear_console(N):-
	nl,
	N1 is N-1,
	clear_console(N1).

%reads the input char and the '\n' character left in input stream
get_char_nl(Input):-
	get_char(Input),
	get_char(_).

get_int(Input):-
	get_code(TempInput),
    get_code(_),
	Input is TempInput - 48.


/* Get element at (row,col) */
get_piece_pos(Column, Row, Message):-
	repeat,
    write(Message), nl,
	write('Column:'),
	get_char_nl(Char),
	letter_to_int(Char, Column),
	write('Row:'),
	get_int(R),
	R =< 8,
	R >= 1,
	Row is R - 1.
