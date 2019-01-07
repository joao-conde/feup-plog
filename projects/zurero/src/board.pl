%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Board Modifiers and Facts Module  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  enemy_player(?Player, ?Enemy)

    Given Player unifies Enemy with other player.
    Given Enemy unifies Player with other player.
*/
enemy_player(white, black).
enemy_player(black, white).


/*  player_stone(?Player, ?PieceNumber)

    Given a Player unifies PieceNumber with the internal board representation of that player's piece.
    Given a PieceNumber unifies Player with the owner.
*/
player_stone(black, 1).
player_stone(white, 2).


/*  direction(?UserInput, ?Direction)

    Given an UserInput unifies Direction with the direction.
    Given a Direction unifies UserInput with the corresponding digit.
*/
direction(1, top).
direction(2, right).
direction(3, bot).
direction(4, left).


/*  board_element(?PieceNumber, ?PieceView)

    Given a PieceNumber unifies PieceView with the console board representation of that piece. 
    Given a PieceView unifies PieceNumber with the internal board piece representation.
*/
board_element(0, '-+-').
board_element(1, '-B-').
board_element(2, '-W-').


/*  initial_board(+Board)

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


/* Line numbers */
line_numbers(['19','18','17','16','15','14','13','12','11','10',' 9',' 8',' 7',' 6',' 5',' 4',' 3',' 2',' 1']).

/* Prints column separators */
print_separator:-
    write('  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |\n').

/* Prints line separators */
print_hline:-
    write('  *-------------------------------------------------------------------------------*\n').

/*  print_board(+Board)

    Prints the line numbers, game board and separators.
*/
print_board(Board) :-
    line_numbers(LineNumbers),
    print_hline,
    print_board_aux(Board, LineNumbers),
    print_separator,
    print_hline,
    write('      A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S\n').

/*  print_board_aux(+Board, +LineNumbers)

    Recursive predicate that prints each line with separators and LineNumbers
*/
print_board_aux([], []).    

print_board_aux([Line|Board],[LineNumb|Remainder]) :-
    print_separator,
    write(LineNumb), write('|-'),
    print_line(Line),
    write('--|'), nl,
    print_board_aux(Board,Remainder).

/*  print_line(+Line)

    Recursive predicate that prints each line
*/
print_line([]).
print_line([Head|Tail]) :-
    board_element(Head,T),
    write('-'),
    write(T),
    print_line(Tail).


/*  move(+Board, -NewBoard, +Coord, top, +Cell)

    Equivalent to a play where a player throws a stone and slides it from the top edge of the board
    towards the bottom.
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard.
*/
move(Board, NewBoard, Coord, top, Piece):-    
    get_leading_pos_col(Board, Coord, LinePos),
    LinePos1 is LinePos + 1,
    LinePos2 is LinePos + 2,
    slide_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, Piece).


/*  move(+Board, -NewBoard, +Coord, bot, +Cell)

    Equivalent to a play where a player throws a stone and slides it from the bottom edge of the board
    towards the top.
    Places the player's stone Cell at the place of impact with another stone, in column Coord unifying
    the result with NewBoard.
*/
move(Board, NewBoard, Coord, bot, Piece):-    
    get_trailing_pos_col(Board, Coord, LinePos),
    LinePos1 is LinePos - 1,
    LinePos2 is LinePos - 2,
    slide_vertically(Board, NewBoard, Coord, LinePos, LinePos1, LinePos2, Piece).


/*  move(+Board, -NewBoard, +Coord, right, +Cell)

    Equivalent to a play where a player throws a stone and slides it from the right edge of the board
    towards the left.
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard.
*/
move(Board, NewBoard, Coord, right, Piece):-
    nth0(Coord, Board, Line),
    get_trailing_pos_line(Line, Pos),
    Pos1 is Pos - 1,
    Pos2 is Pos - 2, 
    slide_horizontally(Board, NewBoard, Coord, Line, Pos, Pos1, Pos2, Piece).


/*  move(+Board, -NewBoard, +Coord, left, +Cell)

    Equivalent to a play where a player throws a stone and slides it from the left edge of the board
    towards the right.
    Places the player's stone Cell at the place of impact with another stone, in row Coord unifying
    the result with NewBoard.
*/
move(Board, NewBoard, Coord, left, Piece):-
    nth0(Coord, Board, Line),
    get_leading_pos_line(Line, Pos),
    Pos1 is Pos + 1,
    Pos2 is Pos + 2, 
    slide_horizontally(Board, NewBoard, Coord, Line, Pos, Pos1, Pos2, Piece).


/*  slide_horizontally(+Board, -NewBoard, +Coord, +Line, _, +Pos1, +Pos2, +Piece)

    Throws a piece horizontally pushing back the stone hit.
*/
slide_horizontally(Board, NewBoard, Coord, Line, _, Pos1, Pos2, Piece):-
    nth0(Pos2, Line, 0),
    nth0(Pos1, Line, PushedPiece),
    push_stones_horizontally(Board, NewBoard, Coord, Pos1, Pos2, PushedPiece, Piece).


/*  slide_horizontally(+Board, -NewBoard, +Coord, +Line, _, +Pos1, +Pos2, +Piece)

    Throws a piece horizontally, stopping it at impact.
*/
slide_horizontally(Board, NewBoard, Coord, Line, Pos, _, Pos2, Piece):-
    \+ nth0(Pos2, Line, 0),
    set_cell(Pos, Coord, Piece, Board, NewBoard).


/*  push_stones_horizontally(+Board, -NewBoard, +Coord, +Pos1, +Pos2, +PushedPiece, +Piece)

    Sets the positions of the two pieces moved.
    The new one in the old one's cell and the old one pushed back.
*/
push_stones_horizontally(Board, NewBoard, Coord, Pos1, Pos2, PushedPiece, Piece):-
    set_cell(Pos1, Coord, Piece, Board, Board2),
    set_cell(Pos2, Coord, PushedPiece, Board2, NewBoard).


/*  slide_vertically(+Board, -NewBoard, +Coord, +LinePos, +LinePos1, +LinePos2, +Piece)

    Throws a piece vertically pushing back the stone hit.
*/
slide_vertically(Board, NewBoard, Coord, _, LinePos1, LinePos2, Piece):-
    nth0(LinePos2, Board, Line2),
    nth0(LinePos1, Board, Line1),
    nth0(Coord, Line2, 0),
    nth0(Coord, Line1, PushedPiece),    
    push_stones_vertically(Board, NewBoard, Coord, LinePos1, LinePos2, PushedPiece, Piece).


/*  slide_vertically(+Board, -NewBoard, +Coord, +LinePos, _, +LinePos2, +Piece)

    Throws a piece vertically, stopping it at impact.
*/
slide_vertically(Board, NewBoard, Coord, LinePos, _, LinePos2, Piece):-
    nth0(LinePos2, Board, Line2),
    \+ nth0(Coord, Line2, 0),
    set_cell(Coord, LinePos, Piece, Board, NewBoard).


/*  push_stones_vertically(+Board, -NewBoard, +Coord, +LinePos1, +LinePos2, +PushedPiece, +Piece)

    Sets the positions of the two pieces moved.
    The new one in the old one's cell and the old one pushed back.
*/
push_stones_vertically(Board, NewBoard, Coord, LinePos1, LinePos2, PushedPiece, Piece):-
    set_cell(Coord, LinePos1, Piece, Board, Board2),
    set_cell(Coord, LinePos2, PushedPiece, Board2, NewBoard).


/*  max_in_a_row_lines(+Board, +Piece, -Max)

    Unifies Max with the maximum in a row Pieces in the Board lines.
*/
max_in_a_row_lines(Board, Piece, Max):-
    cnt_in_a_row_lines(Board, Piece, LinesCnt),
    max_member(Max, LinesCnt).


/*  max_in_a_row_cols(+Board, +Piece, -Max)

    Unifies Max with the maximum in a row Pieces in the Board columns.
*/
max_in_a_row_cols([Line|Board], Piece, Max):-
    cnt_in_a_row_cols([Line|Board], Piece, ColsCnt),
    max_member(Max, ColsCnt).


/*  max_in_a_row_diags(+Board, +Piece, -Max)

    Unifies Max with the maximum in a row Pieces in the Board diagonals.
*/
max_in_a_row_diags(Board, Piece, Max):-
    cnt_in_a_row_diags(Board, Piece, DiagsCnt),
    max_member(Max, DiagsCnt).


/*  cnt_in_a_row_lines(+Board, +Piece, -LinesCnt)

    Unifies LinesCnt with a list for each line in Board with the maximum in a row count of Pieces.
*/
cnt_in_a_row_lines(Board, Piece, LinesCnt):-
    length(Board, Lines),
    findall(Occur, ( (between(1, Lines, I), 
                        nth1(I, Board, Line), 
                        cnt_in_a_row_line(Line, Piece, Occur)) ),
                        LinesCnt).


/*  cnt_in_a_row_cols(+Board, +Piece, -ColsCnt)

    Unifies ColsCnt with a list for each column in Board with the maximum in a row count of Pieces.
*/
cnt_in_a_row_cols([Line|Board], Piece, ColsCnt):-
    length(Line, NumbCols),
    NumbCols0 is NumbCols-1,
    findall(Occur, ( (between(0, NumbCols0, Col),
                        cnt_in_a_row_col([Line|Board], Col, Piece, Occur))),   
                        ColsCnt).


/*  cnt_in_a_row_cols(+Board, +Piece, -DiagsCnt)

    Unifies DiagsCnt with a list for each diagonal in Board with the maximum in a row count of Pieces.
*/
cnt_in_a_row_diags(Board, Piece, DiagsCnt):-
    get_all_diags(Board, Diags),
    length(Diags, NDiags),
    findall(Occur, (between(1, NDiags, I),
                    nth1(I, Diags, Diag),
                    cnt_in_a_row_line(Diag, Piece, Occur)), 
                    DiagsCnt).


/*  cnt_in_a_row_line(+Line, +Piece, -InARow)

    Unifies InARow with the count of in a row Pieces in Line.
*/
cnt_in_a_row_line(Line, Piece, InARow):- 
    aux_cnt_in_a_row_line(Line, Piece, InARow, 0, 0).

/* Base cases */
aux_cnt_in_a_row_line([], _, Max, Cnt, Max):- Cnt =< Max.
aux_cnt_in_a_row_line([], _, Cnt, Cnt, Max):- Cnt > Max.

/* Matching Piece */
aux_cnt_in_a_row_line([Piece|T], Piece, InARow, Cnt, Max):-
    Cnt1 is Cnt+1,
    aux_cnt_in_a_row_line(T, Piece, InARow, Cnt1, Max).

/* Different Piece, reset Cnt, update Max */
aux_cnt_in_a_row_line([H|T], Piece, InARow, Cnt, Max):-
    H \= Piece,
    Cnt > Max,
    aux_cnt_in_a_row_line(T, Piece, InARow, 0, Cnt).

/* Different Piece, reset Cnt, not updating Max */
aux_cnt_in_a_row_line([H|T], Piece, InARow, Cnt, Max):-
    H \= Piece,
    Cnt =< Max,
    aux_cnt_in_a_row_line(T, Piece, InARow, 0, Max).


/*  cnt_in_a_row_col(+Board, +Col, +Piece, -InARow)

    Unifies InARow with the count of in a row Pieces in column Col.
*/
cnt_in_a_row_col(Board, Col, Piece, InARow):- 
    length(Board, NumbLines),
    Col1 is Col+1,
    findall(Cell, (between(1, NumbLines, I), nth1(I, Board, Line), nth1(Col1, Line, Cell)), L),
    cnt_in_a_row_line(L, Piece, InARow).


/*  get_all_diags(+Board, -Diags)

    Unifies Diags with a list of all the Board diagonals.
    BSDiags refer to BACKSLASH diagonals (\).
    FSDiags refer to FORWARDSLASH diagonals (/).
*/
get_all_diags(Board, Diags):-
    nth0(0, Board, FLine),
    length(Board, Lines),
    length(FLine, Cols),
    Lines1 is Lines-1,
    get_diags(1, 1, Board, Cols, Lines1, BsDiags),
    get_diags(-1, Cols, Board, Cols, Lines1, FsDiags),
    append(BsDiags, FsDiags, Diags).


/*  get_diags(+Inc, +ScanStart, +Board, +NCols, +NLines, -SlashDiags)

    Unifies SlashDiags with a list of all the current slash diagonals.
    Board scanning order depends on the ScanStart and Inc. 
    Inc is 1 for FSDiagonals and -1 for BSDiagonals.
    ScanStart is either first column or last, respectively.
*/
get_diags(Inc, ScanStart, Board, NCols, NLines, SlashDiags):-
    findall(DiagEl,
                (between(1, NCols, C), 
                 once(get_diagonal(Inc, Board, 0, C, DiagEl))
                ), 
            TopDiags),
    findall(DiagEl,
                (between(1, NLines, L),
                once(get_diagonal(Inc, Board, L, ScanStart, DiagEl))                    
                ),
            BotDiags),
    append(TopDiags, BotDiags, SlashDiags).


/*  get_diagonal(+Inc, +Board, +Line, +Cols, -SlashDiag)

    Unfies SlashDiag with one diagonal.
*/
get_diagonal(Inc, [_|Board], L, C, BsDiag):-
    L1 is L - 1,
    get_diagonal(Inc, Board, L1, C, BsDiag).

get_diagonal(Inc, Board, 0, C, BsDiag):-
    aux_get_diagonal(Inc, Board, C, BsDiag, []).

aux_get_diagonal(Inc, [FLine|Board], C, BsDiag, Acc):-
    nth1(C, FLine, DiagEl),
    C1 is C + Inc,
    aux_get_diagonal(Inc, Board, C1, BsDiag, [DiagEl|Acc]).

aux_get_diagonal(_, _, _, BsDiag, BsDiag).