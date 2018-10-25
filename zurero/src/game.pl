% Game structure and predicates to operate with it
switch_turn(white, black).
switch_turn(black, white).

% Sufficient to check if row or column has other stones TODO
validate_move(Col, top).
validate_move(Col, bot).
validate_move(Row, left).
validate_move(Row, right).

create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, white, pvp].

% First move, place anywhere
play_game([Board, Player, _]):-
    initial_board(Board),
    print_board(Board),
    get_first_coords(Row, Col), %set piece on board
    player_stone(Player, Cell),
    InvRow is abs(18 - Row), %inverse Row since rows are numbered backwards
    set_cell(Col, InvRow, Cell, Board, NewBoard),
    switch_turn(Player, NextPlayer),
    play_game([NewBoard, NextPlayer, _]).

play_game([Board, Player, pvp]):-
    print_board(Board),
    get_direction(Direction), %1-top, 2-right, 3-bot, 4-left

    get_coord(Coord, Direction), %turns input to 0-based indexes
    validate_move(Coord, Direction),
    %TODO make "throw_stone"
    %make move -> updateBoard
    request_enter,
    switch_turn(Player, NextPlayer),
    play_game([Board, NextPlayer, pvp]).



% play_game([Board, Player, pvb]).

% play_game([Board, Player, bvb]).
