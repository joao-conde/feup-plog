%---------------
valid_moves(Board, ValidMoves):-
    findall(Coord-Dir, valid_move(Board, Coord, Dir), ValidMoves).

bot_move(Diff, Board, Player, NewBoard):-
    player_stone(Player, Piece),
    choose_move(Diff, Board, Piece, MvCoord-MvDir),
    move(Board, NewBoard, MvCoord, MvDir, Piece).


%--------EVAL TEST------
evaluate_move(Board, Coord, Dir, Piece, Eval):-
    move(Board, NewBoard, Coord, Dir, Piece),
    value(NewBoard, Piece, Eval).


value(Board, Piece, Eval):-
    cnt_in_a_row_lines(Board, Piece, LinesCnt),
    cnt_in_a_row_cols(Board, Piece, ColsCnt),
    cnt_in_a_row_diags(Board, Piece, DiagsCnt),
    aux_eval([LinesCnt, ColsCnt, DiagsCnt], Eval).

aux_eval(Cnts, Eval):-
    max_lists(Max, Cnts),
    Max >= 5,
    max_int(MaxInt),
    sum_lists(AuxEval, Cnts),
    Eval is MaxInt + AuxEval.

aux_eval(Cnts, Eval):-
    sum_lists(Eval, Cnts).


%----
choose_move(hard, Board, Piece, BestMoveCoord-BestMoveDir):-
    setof(Eval-Coord-Dir, 
                (valid_move(Board, Coord, Dir),
                 once(evaluate_move(Board, Coord, Dir, Piece, Eval))
                ), 
            AscValidMoves),
    reverse(AscValidMoves, [_-BestMoveCoord-BestMoveDir|_]).

choose_move(easy, Board, _, MvCoord-MvDir):-
    valid_moves(Board, ValidMoves),
    length(ValidMoves, NumbMoves),
    random(0, NumbMoves, MoveIdx), %[Lower, Upper[
    nth0(MoveIdx, ValidMoves, MvCoord-MvDir).
