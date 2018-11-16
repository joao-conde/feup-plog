create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, black, pvp].

create_pvb_game(Game, Diff, StartingPlayer):-
	initial_board(Board),
    Game = [Board, black, pvb, Diff, StartingPlayer].

create_bvb_game(Game, Bot1Lvl, Bot2Lvl):-
    initial_board(Board),
    Game = [Board, black, bvb, Bot1Lvl, Bot2Lvl].

% First move - place in the middle
play_game([Board, Player|Other]):-
    initial_board(Board),
    clear_console,
    player_stone(Player, Piece),
    set_cell(9, 9, Piece, Board, NewBoard), %TODO: not hardcoded 
    switch_turn(Player, NextPlayer),
    print_board(NewBoard),
    write('First piece placed in the middle of the board for '), write(Player), write(' player\n'),
    request_enter,
    play_game([NewBoard, NextPlayer|Other]).


%---PvP---
play_game([Board, Player, pvp]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    player_move(Board, Player, NewBoard),
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, Player),
    check_game_over(NewBoard, NextPlayer),
    play_game([NewBoard, NextPlayer, pvp]).


%---PvB---
play_game([Board, Player, pvb, Diff, bot]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    request_enter,
    bot_move(Diff, Board, Player, NewBoard),
    
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, Player),
    check_game_over(NewBoard, NextPlayer),
    
    clear_console,
    print_board(NewBoard),
    print_player_turn(NextPlayer),
    player_move(NewBoard, NextPlayer, NewBoard2),
    check_game_over(NewBoard2, NextPlayer),
    check_game_over(NewBoard2, Player),
    play_game([NewBoard2, Player, pvb, Diff, bot]).


play_game([Board, Player, pvb, Diff, player]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    player_move(Board, Player, NewBoard),
    
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, Player),
    check_game_over(NewBoard, NextPlayer),
    
    clear_console,
    print_board(NewBoard),
    print_player_turn(NextPlayer),
    request_enter,
    bot_move(Diff, NewBoard, NextPlayer, NewBoard2),
    check_game_over(NewBoard2, NextPlayer),
    check_game_over(NewBoard2, Player),
    play_game([NewBoard2, Player, pvb, Diff, player]).

%TODO add 2 check game overs everytime

%---BvB---
play_game([Board, Player, bvb, Bot1Lvl, Bot2Lvl]):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    request_enter,
    bot_move(Bot1Lvl, Board, Player, NewBoard),
    print_board(NewBoard),
    request_enter,
    switch_turn(Player, NextPlayer),
    check_game_over(NewBoard, Player),
    check_game_over(NewBoard, NextPlayer),
    print_board(NewBoard),
    bot_move(Bot2Lvl, NewBoard, NextPlayer, NewBoard2),
    check_game_over(NewBoard2, NextPlayer),
    play_game([NewBoard2, Player, bvb, Bot1Lvl, Bot2Lvl]).



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