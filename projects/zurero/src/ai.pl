%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Artificial Intelligence Module  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  bot_move(+Diff, +Board, +Player, -NewBoard)

    Performs the bot move unifying resulting board with NewBoard.
*/
bot_move(Diff, Board, Player, NewBoard):-
    player_stone(Player, Piece),
    choose_move(Diff, Board, Piece, MvCoord-MvDir),
    move(Board, NewBoard, MvCoord, MvDir, Piece).


/*  choose_move(hard, +Board, +Piece, -Move)

    Chooses the best move, according to the evaluation predicate
*/
choose_move(hard, Board, Piece, BestMoveCoord-BestMoveDir):-
    setof(Eval-Coord-Dir, 
                (valid_move(Board, Coord, Dir),
                 once(evaluate_move(Board, Coord, Dir, Piece, Eval))
                ), 
            AscValidMoves),
    reverse(AscValidMoves, [_-BestMoveCoord-BestMoveDir|_]).


/*  choose_move(easy, +Board, +Piece, -Move)

    Chooses a random move.
*/
choose_move(easy, Board, _, MvCoord-MvDir):-
    valid_moves(Board, ValidMoves),
    length(ValidMoves, NumbMoves),
    random(0, NumbMoves, MoveIdx), %[Lower, Upper[
    nth0(MoveIdx, ValidMoves, MvCoord-MvDir).


/*  evaluate_move(+Board, +Coord, +Dir, +Piece, -Eval)

    Evaluates a move.
    In order to do so, performs the move and evaluates resulting board.
*/
evaluate_move(Board, Coord, Dir, Piece, Eval):-
    move(Board, NewBoard, Coord, Dir, Piece),
    value(NewBoard, Piece, MyEval),
    player_stone(Player, Piece),
    enemy_player(Player, Enemy),
    player_stone(Enemy, EnemyPiece),
    value(NewBoard, EnemyPiece, EnemyEval),
    Eval is MyEval - EnemyEval.


/*  value(+Board, +Piece, -Eval)

    Evaluates a Board.
    It takes into account the number of in a row pieces.
    It gives it a huge evaluation if there are 5 in a row, because it is a winning board.
*/
value(Board, Piece, Eval):-
    cnt_in_a_row_lines(Board, Piece, LinesCnt),
    cnt_in_a_row_cols(Board, Piece, ColsCnt),
    cnt_in_a_row_diags(Board, Piece, DiagsCnt),
    aux_eval([LinesCnt, ColsCnt, DiagsCnt], Eval).

/* Case where there are 5 (or more) in a row pieces --> game winning Board */
aux_eval(Cnts, Eval):-
    max_lists(Max, Cnts),
    Max >= 5,
    max_int(MaxInt),
    sum_lists(AuxEval, Cnts),
    Eval is MaxInt + AuxEval.

/* No 5 in a row sequence */
aux_eval(Cnts, Eval):-
    sum_lists(Eval, Cnts).