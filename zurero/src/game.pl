create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, black, pvp].

create_pvb_game(Game, Diff):-
	initial_board(Board),
    Game = [Board, black, pvb, Diff].

create_bvb_game(Game):-
    initial_board(Board),
    Game = [Board, black, bvb].

% First move - place in the middle
play_game([Board, Player|Other]):-
    initial_board(Board),
    clear_console,
    player_stone(Player, Piece),
    set_cell(9, 9, Piece, Board, NewBoard),
    switch_turn(Player, NextPlayer),
    print_board(NewBoard),
    write('First piece placed in the middle of the board for '), write(Player), write(' player\n'),
    request_enter,
    play_game([NewBoard, NextPlayer|Other]).


%---PVP---
play_game([Board, Player, pvp]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    player_move(Board, Player, NewBoard),
    check_game_over(NewBoard, Player),
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, NextPlayer),
    play_game([NewBoard, NextPlayer, pvp]).


%---PvB---
play_game([Board, Player, pvb, Diff]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    request_enter,
    bot_move(Diff, Board, Player, NewBoard),
    print_board(NewBoard),
    request_enter,
    check_game_over(NewBoard, Player),
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, NextPlayer),
    print_board(NewBoard),
    player_move(NewBoard, NextPlayer, NewBoard2),
    check_game_over(NewBoard2, NextPlayer),
    play_game([NewBoard2, Player, pvb, Diff]).



%---BvB---
play_game([Board, Player, bvb]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    request_enter,
    bot_move(easy, Board, Player, NewBoard),
    print_board(NewBoard),
    request_enter,
    check_game_over(NewBoard, Player),
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, NextPlayer),
    print_board(NewBoard),
    bot_move(hard, NewBoard, NextPlayer, NewBoard2),
    check_game_over(NewBoard2, NextPlayer),
    play_game([NewBoard2, Player, bvb]).



player_first_move(Board, Player, NewBoard):-
    get_first_coords(Line, Col), 
    player_stone(Player, Piece),
    set_cell(Col, Line, Piece, Board, NewBoard).


player_move(Board, Player, NewBoard):-
    get_direction(Direction), %1-top, 2-right, 3-bot, 4-left
    get_coord(Coord, Direction), %turns input to 0-based indexes
    valid_move(Board, Coord, Direction),
    player_stone(Player, Piece),
    throw_stone(Board, NewBoard, Coord, Direction, Piece).


check_game_over(Board, Player):-
    check_win(Board, Player),
    clear_console,
    print_board(Board),
    write(Player), write(' player won\n'),
    request_enter,
    game_mode_menu.

check_game_over(_, _).