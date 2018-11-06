create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, black, pvp].

% First move, place anywhere
play_game([Board, Player, _]):-
    clear_console,
    initial_board(Board),
    print_board(Board),
    get_first_coords(Line, Col), 
    player_stone(Player, Piece),
    set_cell(Col, Line, Piece, Board, NewBoard),
    switch_turn(Player, NextPlayer),
    play_game([NewBoard, NextPlayer, _]).

play_game([Board, Player, pvp]):-
    clear_console,
    print_board(Board),
    get_direction(Direction), %1-top, 2-right, 3-bot, 4-left

    get_coord(Coord, Direction), %turns input to 0-based indexes
    
    valid_move(Board, Coord, Direction),
    player_stone(Player, Piece),
    throw_stone(Board, NewBoard, Coord, Direction, Piece),
    switch_turn(Player, NextPlayer),
    play_game([NewBoard, NextPlayer, pvp]).



% play_game([Board, Player, pvb]).

% play_game([Board, Player, bvb]).

