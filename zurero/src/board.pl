% Game structure and predicates to operate with it
switch_turn(white, black).
switch_turn(black, white).

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
    throw_stone(-Board, +NewBoard, -Coord, top, -Cell)

    Equivalent to a play where a player throws a stone and slide_horizontallys it from the top edge of the board
    towards the bottom
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, top, Piece):-    
    get_leading_pos_col(Board, Coord, LinePos),
    LinePos1 is LinePos + 1,
    LinePos2 is LinePos + 2,
    slide_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, Piece).


/**
    throw_stone(-Board, +NewBoard, -Coord, bot, -Cell)

    Equivalent to a play where a player throws a stone and slide_horizontallys it from the bottom edge of the board
    towards the top
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, bot, Piece):-    
    get_trailing_pos_col(Board, Coord, LinePos),
    LinePos1 is LinePos - 1,
    LinePos2 is LinePos - 2,
    slide_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, Piece).


/**
    throw_stone(-Board, +NewBoard, -Coord, right, -Cell)

    Equivalent to a play where a player throws a stone and slide_horizontallys it from the right edge of the board
    towards the left
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, right, Piece):-
    nth0(Coord, Board, Line),
    get_trailing_pos_line(Line, Pos),
    Pos1 is Pos - 1,
    Pos2 is Pos - 2, 
    slide_horizontally(Board, NewBoard, Coord, Line, Pos, Pos1, Pos2, Piece).

/**
    throw_stone(-Board, +NewBoard, -Coord, left, -Cell)

    Equivalent to a play where a player throws a stone and slide_horizontallys it from the left edge of the board
    towards the right
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard
*/
throw_stone(Board, NewBoard, Coord, left, Piece):-
    nth0(Coord, Board, Line),
    get_leading_pos_line(Line, Pos),
    Pos1 is Pos + 1,
    Pos2 is Pos + 2, 
    slide_horizontally(Board, NewBoard, Coord, Line, Pos, Pos1, Pos2, Piece).

%-------------
slide_horizontally(Board, NewBoard, Coord, Line, Pos, Pos1, Pos2, Piece):-
    nth0(Pos2, Line, 0),
    nth0(Pos1, Line, PushedPiece),
    push_stones_horizontally(Board, NewBoard, Coord, Pos, Pos1, Pos2, PushedPiece, Piece).

slide_horizontally(Board, NewBoard, Coord, Line, Pos, _, Pos2, Piece):-
    \+ nth0(Pos2, Line, 0),
    set_cell(Pos, Coord, Piece, Board, NewBoard).


push_stones_horizontally(Board, NewBoard, Coord, _, Pos1, Pos2, PushedPiece, Piece):-
    set_cell(Pos1, Coord, Piece, Board, Board2),
    set_cell(Pos2, Coord, PushedPiece, Board2, NewBoard).

%-------------
slide_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, Piece):-
    nth0(LinePos2, Board, Line2),
    nth0(LinePos1, Board, Line1),
    nth0(Coord, Line2, 0),
    nth0(Coord, Line1, PushedPiece),    
    push_stones_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, PushedPiece, Piece).

slide_vertically(Board, NewBoard, Coord, LinePos, _, LinePos2, Piece):-
    nth0(LinePos2, Board, Line2),
    \+ nth0(Coord, Line2, 0),
    set_cell(Coord, LinePos, Piece, Board, NewBoard).

push_stones_vertically(Board, NewBoard, Coord, _, LinePos1, LinePos2, PushedPiece, Piece):-
    set_cell(Coord, LinePos1, Piece, Board, Board2),
    set_cell(Coord, LinePos2, PushedPiece, Board2, NewBoard).


%VALID MOVE
valid_move(Board, Coord, top):-
    valid_move_col(Board, Coord).

valid_move(Board, Coord, bot):-
    valid_move_col(Board, Coord).

valid_move(Board, Coord, left):-
    valid_move_line(Board, Coord).

valid_move(Board, Coord, right):-
    valid_move_line(Board, Coord).

%----
valid_move_line(Board, Coord):-
    nth0(Coord, Board, Line),
    \+ empty(Line).


%----
valid_move_col([Line|_], Coord):-
    \+ nth0(Coord, Line, 0).

valid_move_col([Line|Board], Coord):-
    nth0(Coord, Line, 0),
    valid_move_col(Board, Coord).