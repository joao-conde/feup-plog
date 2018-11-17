/*  letter_to_int(+Letter, -Num)
    
    Instantiates Num with the corresponding number assuming A/a is 0 and Z/z is 25.
    Works for both lowercase and uppercase letters.
*/
letter_to_int(Letter, Num):-
    Letter @>= 'A',
    Letter @=< 'Z',
    char_code('A', AsciiA),
    char_code(Letter, AsciiL),
    Num is AsciiL - AsciiA.

letter_to_int(Letter, Num):-
    Letter @>= 'a',
    Letter @=< 'z',
    char_code('a', AsciiA),
    char_code(Letter, AsciiL),
    Num is AsciiL - AsciiA.


/*  get_leading_pos_line(+Line, -Position)
    
    Finds the leading empty position in a line i.e. the empty cell at the left of a piece.
    Position is matched with that line index or -1 if no leading space exists.
*/
get_leading_pos_line(Line, Position):-
    aux_get_leading_pos_line(Line, Position, -1).

/* Recursion base case */
aux_get_leading_pos_line([], Cnt, Cnt).

/* Found a piece in the next position, relative to current Cnt */
aux_get_leading_pos_line([Piece|_], Cnt, Cnt):-
    player_stone(_, Piece).

/* Empty cell in the next position, increment Cnt */
aux_get_leading_pos_line([_|T], Position, Cnt):-
    Cnt1 is Cnt + 1,
    aux_get_leading_pos_line(T, Position, Cnt1).


/*  get_trailing_pos_line(+Line, -Position)
    
    Finds the trailing empty position in a line i.e. the empty cell at the right of a piece.
    Position is matched with that line index or 19 if no trailing space exists.
*/
get_trailing_pos_line(Line, Position):-
    reverse(Line, RevLine),
    get_leading_pos_line(RevLine, RevPos),
    length(Line, LineLen),
    LineLen1 is LineLen - 1,
    Position is LineLen1 - RevPos.



/*  get_leading_pos_col(+Board, +ElColPos, -LinePos)
    
    Finds the leading empty position in a column i.e. the empty cell above of a piece.
    LinePos is matched with the line index where the element at ElColPos is the leading space.
    -1 if none exists.
*/
get_leading_pos_col(Board, ElColPos, LinePos):-
    aux_get_leading_pos_col(Board, ElColPos, LinePos, -1).

/* Recursion base case */
aux_get_leading_pos_col([], _, Cnt, Cnt).

/* Found a piece below of (Cnt, ElColPos) */
aux_get_leading_pos_col([Line|_], ElColPos, Cnt, Cnt):-
    \+ nth0(ElColPos, Line, 0).

/* Found an empty space below of (Cnt, ElColPos) */
aux_get_leading_pos_col([Line|T], ElColPos, LinePos, Cnt):-
    nth0(ElColPos, Line, 0),
    Cnt1 is Cnt+1,
    aux_get_leading_pos_col(T, ElColPos, LinePos, Cnt1).


/*  get_trailing_pos_col(+Board, +ElColPos, -LinePos)
    
    Finds the trailing empty position in a column i.e. the empty cell below of a piece.
    LinePos is matched with the line index where the element at ElColPos is the trailing space.
    -1 if none exists.
*/
get_trailing_pos_col(Board, ElColPos, LinePos):-
    reverse(Board, RevBoard),
    get_leading_pos_col(RevBoard, ElColPos, RevLinePos),
    length(Board, BoardLen),
    BoardLen1 is BoardLen - 1,
    LinePos is BoardLen1 - RevLinePos.


/*  set_cell(-Col, -Row, -Elem, -Board, +NewBoard)

    Unifies NewBoard with a Board where at position (Row, Col) there is piece Elem.
    Makes calls to predicate set_cell_list for each of the rows.
*/
set_cell(ElemCol, 0, NewElem, [LineAtTheHead|RemainingLines], [NewLineAtTheHead|RemainingLines]):-
	set_cell_list(ElemCol, NewElem, LineAtTheHead, NewLineAtTheHead).

set_cell(ElemCol, ElemLine, NewElem, [LineAtTheHead|RemainingLines], [LineAtTheHead|ResultRemainingLines]):-
	ElemLine > 0,
	ElemLine1 is ElemLine-1,
	set_cell(ElemCol, ElemLine1, NewElem, RemainingLines, ResultRemainingLines).

/*  set_cell_list(-Idx, -Elem, -Row, +UpdatedRow)

    Unifies UpdatedRow with a Row where at position Idx there is piece Elem.
*/
set_cell_list(0, Elem, [_|L], [Elem|L]).
set_cell_list(I, Elem, [H|L], [H|ResL]):-
	I > 0,
	I1 is I-1,
	set_cell_list(I1, Elem, L, ResL).


/*  empty(+Line)

    Succeeds if Line is consired empy, meaning it only consits of white spaces, no pieces
*/
empty(Line):- 
    player_stone(black, BlackPiece),
    player_stone(white, WhitePiece),
    \+ member(BlackPiece, Line),
    \+ member(WhitePiece, Line).


/*  between(+N, +M, -K)

    Generates a number in range [N, M] unifying it with K.
*/
between(N, M, K):- 
    N =< M, 
    K = N.

between(N, M, K):- 
    N < M, 
    N1 is N+1, 
    between(N1, M, K).


/*  sum_lists(-Sum, +Lists)

    Being Lists a list of lists, Sum is unified with the sum of all those lists.
*/
sum_lists(Sum, Lists):-
    aux_sum_lists(Sum, Lists, 0).

/* Recursion base case */
aux_sum_lists(Sum, [], Sum).

/* Update accumulator with sum of current list */
aux_sum_lists(Sum, [List|T], Acc):-
    sumlist(List, SumList),
    Acc1 is Acc + SumList,
    aux_sum_lists(Sum, T, Acc1).


/*  max_lists(-Max, +Lists)

    Being Lists a list of lists, Max is unified with the maximum value of all those lists.
*/
max_lists(Max, Lists):-
    aux_max_lists(Max, Lists, -1).

/* Recursion base case */
aux_max_lists(Max, [], Max).

/* Max of current list is bigger than current known Max */
aux_max_lists(Max, [List|T], AuxMax):-
    max_member(TempMax, List),
    TempMax > AuxMax,
    aux_max_lists(Max, T, TempMax).

/* Max of currnet list is smaller or equal than current known Max */
aux_max_lists(Max, [List|T], AuxMax):-
    max_member(TempMax, List),
    TempMax =< AuxMax,
    aux_max_lists(Max, T, AuxMax).