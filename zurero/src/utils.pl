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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	INPUT REQUEST AND HANDLING	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_char_nl(Input):-
	get_char(Input),
	get_char(_).

request_enter:-
	write('Press <Enter> to continue.\n'),
	get_char(_), !.

print_direction_msg:-
	nl, write('Possible sides:\n'),
	write('1 - top\n'),
	write('2 - right\n'),
	write('3 - bot\n'),
	write('4 - left\n').

print_coord_msg:-
	write('Where to throw it?\t').

print_first_move_msg:-
	write('First move of the game, place stone anywhere\n').

get_int(Input):-
	get_code(TempInput),
  	get_code(_),
	Input is TempInput - 48.

get_direction(Direction):-
	repeat,
	print_direction_msg,
	get_int(Input),
	direction(Input, Direction).

get_coord(Col, top):-
	repeat,
	print_coord_msg,
	get_char_nl(Col),
	letter_to_int(Col, Num),
	Num >= 0,
	Num =< 18.

get_coord(Col, bot):-
	repeat,
	print_coord_msg,
	get_char_nl(Col),
	letter_to_int(Col, Num),
	Num >= 0,
	Num =< 18.

get_coord(Row, left):-
	repeat,
	print_coord_msg,
	get_int(Row1),
	Row1 >= 1, 
	Row1 =< 19,
	Row is Row1 - 1.

get_coord(Row, right):-
	repeat,
	print_coord_msg,
	get_int(Row1),
	Row1 >= 1, 
	Row1 =< 19,
	Row is Row1 - 1.

get_first_coords(Row, Col):-
	repeat,
	print_first_move_msg,
	write('Row?\t'),
	get_int(Row1),
	Row is Row1-1,
	write('Column?\t'),
	get_char_nl(ColLetter),
	letter_to_int(ColLetter, Col).
