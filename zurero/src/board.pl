/**
    player_stone(-Player, +PieceNumber)

    Given a Player unifies PieceNumber with the internal board representation of that player's piece.
*/
player_stone(black, 1).
player_stone(white, 2).

/**
    direction(-UserInput, +Direction)

    Maps each of the 4 possible UserInput directions selected by the user to a Direction.
*/
direction(1, top).
direction(2, right).
direction(3, bot).
direction(4, left).

/**
    board_element(-PieceNumber, PieceView)

    Given a PieceNumber unifies PieceView with the console board representation of that piece. 
*/
board_element(0, '-+-').
board_element(1, '-B-').
board_element(2, '-W-').

/**
    initial_board(-Board)

    Unifies Board with the initial empy game board (19x19)
*/
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


%%%%%%%%%%%%%%%%%%%%%%
%   BOARD DISPLAY    %
%%%%%%%%%%%%%%%%%%%%%%
line_numbers(['19','18','17','16','15','14','13','12','11','10',' 9',' 8',' 7',' 6',' 5',' 4',' 3',' 2',' 1']).

print_separator:-
    write('  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |\n').

print_hline:-
    write('  *-------------------------------------------------------------------------------*\n').

/**
    print_board(-Board)

    Prints the line numbers, game board and separators.
*/
print_board(Board) :-
    line_numbers(LineNumbers),
    print_hline,
    print_board_aux(Board, LineNumbers),
    print_separator,
    print_hline,
    write('      A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S\n').

/**
    print_board_aux(-Board, -LineNumbers)

    Recursive predicate that prints each line with separators and LineNumbers
*/
print_board_aux([],[]).    

print_board_aux([Line|Board],[LineNumb|Remainder]) :-
    print_separator,
    write(LineNumb), write('|-'),
    print_line(Line),
    write('--|'), nl,
    print_board_aux(Board,Remainder).

/**
    print_line(-Line)

    Recursive predicate that prints each line
*/
print_line([]).
print_line([Head|Tail]) :-
    board_element(Head,T),
    write('-'),
    write(T),
    print_line(Tail).


%%%%%%%%%%%%%%%%%%%%%%%
%   BOARD MODIFIERS   %
%%%%%%%%%%%%%%%%%%%%%%%

/**
    set_cell(-Col, -Row, -Elem, -Board, +NewBoard)

    Unifies NewBoard with a Board where at position (Row, Col) there is piece Elem.
    Makes calls to predicate set_cell_list for each of the rows.
*/
set_cell(ElemCol, 0, NewElem, [RowAtTheHead|RemainingRows], [NewRowAtTheHead|RemainingRows]):-
	set_cell_list(ElemCol, NewElem, RowAtTheHead, NewRowAtTheHead).

set_cell(ElemCol, ElemRow, NewElem, [RowAtTheHead|RemainingRows], [RowAtTheHead|ResultRemainingRows]):-
	ElemRow > 0,
	ElemRow1 is ElemRow-1,
	set_cell(ElemCol, ElemRow1, NewElem, RemainingRows, ResultRemainingRows).

/**
    set_cell_list(-Idx, -Elem, -Row, +UpdatedRow)

    Unifies UpdatedRow with a Row where at position Idx there is piece Elem
*/
set_cell_list(0, Elem, [_|L], [Elem|L]).
set_cell_list(I, Elem, [H|L], [H|ResL]):-
	I > 0,
	I1 is I-1,
	set_cell_list(I1, Elem, L, ResL).

% TODO MAKE COLLIDED STONE GO BACK AND OCCUPY ITS SPOT
/**
    throw_stone(-Board, +NewBoard, -Coord, top, -Cell)

    Equivalent to a play where a player throws a stone and slides it from the top edge of the board
    towards the bottom
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, top, Cell):-    
    get_leading_pos_col(Board, Coord, LinePos),
    set_cell(Coord, LinePos, Cell, Board, NewBoard).


/**
    throw_stone(-Board, +NewBoard, -Coord, bot, -Cell)

    Equivalent to a play where a player throws a stone and slides it from the bottom edge of the board
    towards the top
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, bot, Cell):-    
    get_trailing_pos_col(Board, Coord, LinePos),
    set_cell(Coord, LinePos, Cell, Board, NewBoard).


/**
    throw_stone(-Board, +NewBoard, -Coord, left, -Cell)

    Equivalent to a play where a player throws a stone and slides it from the left edge of the board
    towards the right
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, left, Cell):-
    nth0(Coord, Board, Line),
    get_leading_pos_line(Line, Position),
    set_cell(Position, Coord, Cell, Board, NewBoard).


/**
    throw_stone(-Board, +NewBoard, -Coord, right, -Cell)

    Equivalent to a play where a player throws a stone and slides it from the right edge of the board
    towards the left
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, right, Cell):-
    nth0(Coord, Board, Line),
    get_trailing_pos_line(Line, Position),
    set_cell(Position, Coord, Cell, Board, NewBoard).
    

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

    





