% Predicate to convert letters from rows or columns to an integer
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

% Clears console by printing multiple newlines 
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

% Waits for user <Enter> keypress
request_enter:-
	write('Press <Enter> to continue.\n'),
	get_char(_), !.

% Show available sides to choose from 
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

/**
    get_int(-Input)

    Receive integer from user input.
*/
get_int(Input):-
	get_code(TempInput),
  	get_code(_),
	Input is TempInput - 48.

/**
    get_direction(-Direction)

    Get user chosen direction.
*/
get_direction(Direction):-
	repeat,
	print_direction_msg,
	get_int(Input),
	direction(Input, Direction).

/**
    get_vertical_coord(-Col)

    Gets vertical coord from user input. 
*/
get_vertical_coord(Col):-
	repeat,
	print_coord_msg,
	get_char_nl(ColChar),
	letter_to_int(ColChar, Col),
	Col >= 0,
	Col =< 18.

/**
    get_vertical_coord(-Col)

    Gets horizontal coord from user input. 
*/
get_horizontal_coord(InvLine):-
	repeat,
	print_coord_msg,
	get_int(Line1),
	Line1 >= 1, 
	Line1 =< 19,
	Line is Line1 - 1,
	InvLine is abs(18 - Line).

/**
    get_coord(-Col, -Direction)

    Calls correct predicate based on direction. 
*/
get_coord(Col, top):-
	get_vertical_coord(Col).

get_coord(Col, bot):-
	get_vertical_coord(Col).

get_coord(InvLine, left):-
	get_horizontal_coord(InvLine).

get_coord(InvLine, right):-
	get_horizontal_coord(InvLine).

/**
    get_first_coords(-Row, -Col)

    Gets coords for the first piece.
*/
get_first_coords(InvLine, Col):-
	repeat,
	print_first_move_msg,
	write('Row?\t'),
	get_int(Line1),
	Line is Line1-1,
	InvLine is abs(18 - Line), %inverse Line since lines are numbered backwards (visually)
	write('Column?\t'),
	get_char_nl(ColLetter),
	letter_to_int(ColLetter, Col).


/* 
    Leading available position in a line 
    -1 if there is a piece at the start of the line
    or position for leading empty space of line
*/
get_leading_pos_line(Line, Position):-
    aux_get_leading_pos_line(Line, Position, -1).

aux_get_leading_pos_line([], Cnt, Cnt).
aux_get_leading_pos_line([1|_], Cnt, Cnt).
aux_get_leading_pos_line([2|_], Cnt, Cnt).
    
aux_get_leading_pos_line([_|T], Position, Cnt):-
    Cnt1 is Cnt+1,
    aux_get_leading_pos_line(T, Position, Cnt1).


/* 
    Trailing available position in a line 
    19 if there is a piece at the end of the line
    or position for trailing empty space of line
*/
get_trailing_pos_line(Line, Position):-
    reverse(Line, RevLine),
    aux_get_trailing_pos_line(RevLine, Position, 19).

aux_get_trailing_pos_line([], Cnt, Cnt).
aux_get_trailing_pos_line([1|_], Cnt, Cnt).
aux_get_trailing_pos_line([2|_], Cnt, Cnt).
    
aux_get_trailing_pos_line([_|T], Position, Cnt):-
    Cnt1 is Cnt-1,
    aux_get_trailing_pos_line(T, Position, Cnt1).

/* 
    Leading available position in a column 
    -1 if there is a piece at the start of the column
    or position for leading empty space of column
*/
get_leading_pos_col(Board, ElColPos, LinePos):-
    aux_get_leading_pos_col(Board, ElColPos, LinePos, -1).

aux_get_leading_pos_col([], _, Cnt, Cnt).
aux_get_leading_pos_col([Line|_], ElColPos, Cnt, Cnt):-
    \+ nth0(ElColPos, Line, 0).

aux_get_leading_pos_col([Line|T], ElColPos, LinePos, Cnt):-
    nth0(ElColPos, Line, 0), %empty
    Cnt1 is Cnt+1,
    aux_get_leading_pos_col(T, ElColPos, LinePos, Cnt1).

/* 
    Trailing available position in a column
    19 if there is a piece at the end of the column
    or position for trailing empty space of column
*/
get_trailing_pos_col(Board, ElColPos, LinePos):-
    reverse(Board, RevBoard),
    aux_get_trailing_pos_col(RevBoard, ElColPos, LinePos, 19).

aux_get_trailing_pos_col([], _, Cnt, Cnt).
aux_get_trailing_pos_col([Line|_], ElColPos, Cnt, Cnt):-
    \+ nth0(ElColPos, Line, 0).

aux_get_trailing_pos_col([Line|T], ElColPos, LinePos, Cnt):-
    nth0(ElColPos, Line, 0), %empty
    Cnt1 is Cnt-1,
    aux_get_trailing_pos_col(T, ElColPos, LinePos, Cnt1).


/**
    set_cell(-Col, -Row, -Elem, -Board, +NewBoard)

    Unifies NewBoard with a Board where at position (Row, Col) there is piece Elem.
    Makes calls to predicate set_cell_list for each of the rows.
*/
set_cell(ElemCol, 0, NewElem, [LineAtTheHead|RemainingLines], [NewLineAtTheHead|RemainingLines]):-
	set_cell_list(ElemCol, NewElem, LineAtTheHead, NewLineAtTheHead).

set_cell(ElemCol, ElemLine, NewElem, [LineAtTheHead|RemainingLines], [LineAtTheHead|ResultRemainingLines]):-
	ElemLine > 0,
	ElemLine1 is ElemLine-1,
	set_cell(ElemCol, ElemLine1, NewElem, RemainingLines, ResultRemainingLines).

/**
    set_cell_list(-Idx, -Elem, -Row, +UpdatedRow)

    Unifies UpdatedRow with a Row where at position Idx there is piece Elem
*/
set_cell_list(0, Elem, [_|L], [Elem|L]).
set_cell_list(I, Elem, [H|L], [H|ResL]):-
	I > 0,
	I1 is I-1,
	set_cell_list(I1, Elem, L, ResL).


%no pieces (all 0's)
empty(Line):- 
    \+ member(1, Line),
    \+ member(2, Line).