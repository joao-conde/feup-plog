request_enter:-
	write('Press <Enter> to continue.'), nl,
	wait_for_enter, !.

wait_for_enter:-
	get_char(_).

translate(0,'-+-').
translate(1,'-B-'). %Black Pieces
translate(2,'-W-'). %White Pieces

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
clear_console:- clear_console(20), !.
clear_console(0).
clear_console(N):-
	nl,
	N1 is N-1,
	clear_console(N1).

%reads the input char and the '\n' character left in input stream
get_char_nl(Input):-
	get_char(Input),
	get_char(_).

/* Starting game board */
initial_board([[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).