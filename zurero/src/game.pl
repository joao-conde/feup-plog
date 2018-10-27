% Game structure and predicates to operate with it
switch_turn(white, black).
switch_turn(black, white).

% Sufficient to check if row or column has other stones TODO
% validate_move(Col, top).
% validate_move(Col, bot).
% validate_move(Row, left).
% validate_move(Row, right).

create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, white, pvp].

% First move, place anywhere
play_game([Board, Player, _]):-
    clear_console,
    initial_board(Board),
    print_board(Board),
    get_first_coords(Row, Col), 
    player_stone(Player, Cell),
    set_cell(Col, Row, Cell, Board, NewBoard),
    switch_turn(Player, NextPlayer),
    play_game([NewBoard, NextPlayer, _]).

play_game([Board, Player, pvp]):-
    clear_console,
    print_board(Board),
    get_direction(Direction), %1-top, 2-right, 3-bot, 4-left

    get_coord(Coord, Direction), %turns input to 0-based indexes
    
    %validate_move(Coord, Direction),
    %TODO make "throw_stone"
    %make move -> updateBoard
    player_stone(Player, Cell),
    throw_stone(Board, NewBoard, Coord, Direction, Cell),
    switch_turn(Player, NextPlayer),
    play_game([NewBoard, NextPlayer, pvp]).



% play_game([Board, Player, pvb]).

% play_game([Board, Player, bvb]).
