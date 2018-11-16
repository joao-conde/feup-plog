%---------------
bot_move(easy, Board, Player, NewBoard):-
    findall(Coord-Dir, valid_move(Board, Coord, Dir), ValidMoves),
    length(ValidMoves, NumbMoves),
    random(0, NumbMoves, MoveIdx), %[Lower, Upper[
    nth0(MoveIdx, ValidMoves, MvCoord-MvDir),
    player_stone(Player, Piece),
    throw_stone(Board, NewBoard, MvCoord, MvDir, Piece).


bot_move(hard, Board, Player, NewBoard):-
    player_stone(Player, Piece),
    setof(Eval-Coord-Dir, 
                (valid_move(Board, Coord, Dir),
                 once(evaluate_move(Board, Coord, Dir, Piece, Eval))
                ), 
            AscValidMoves),
    write(AscValidMoves), nl,
    reverse(AscValidMoves, [_-BestMoveCoord-BestMoveDir|_]),
    throw_stone(Board, NewBoard, BestMoveCoord, BestMoveDir, Piece).


% bot_move(superhard, Board, Player, NewBoard):-
%     player_stone(Player, Piece),
%     setof(Eval-Coord-Dir, 
%                 (valid_move(Board, Coord, Dir),
%                  once(evaluate_move_enhanced(Board, Coord, Dir, Piece, Eval))
%                 ), 
%             AscValidMoves),
%     write(AscValidMoves), nl,
%     reverse(AscValidMoves, [_-BestMoveCoord-BestMoveDir|_]),
%     throw_stone(Board, NewBoard, BestMoveCoord, BestMoveDir, Piece).

%--------EVAL TEST------
evaluate_move(Board, Coord, Dir, Piece, Eval):-
    throw_stone(Board, NewBoard, Coord, Dir, Piece),
    evaluate(NewBoard, Piece, Eval).

% evaluate_move_enhanced(Board, Coord, Dir, Piece, Eval):-
%     throw_stone(Board, NewBoard, Coord, Dir, Piece),
%     evaluate_enhanced(NewBoard, Piece, Eval).

evaluate(Board, Piece, Eval):-
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


% evaluate_enhanced(Board, Piece, Eval):-
%     evaluate(Board, Piece, MyEval),
%     player_stone(Me, Piece),
%     switch_turn(Me, Enemy),
%     player_stone(Enemy, EnemyPiece),
%     evaluate(Board, EnemyPiece, EnemyEval),
%     Eval is MyEval - EnemyEval.