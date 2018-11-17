/*  create_pvp_game(-Game)

    Creates a PvP (Player versus Player) game.
    Game is unified with a list with an initial board, the first player color and game mode.
*/
create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, black, pvp].


/*  create_pvb_game(-Game, +Diff, +StartingPlayer)

    Creates a PvB (Player versus Bot) game.
    Game is unified with a list with an initial board, the first player color, the game mode,
    the bot gameplay level (or difficulty) and the player who starts.
*/
create_pvb_game(Game, Diff, StartingPlayer):-
	initial_board(Board),
    Game = [Board, black, pvb, Diff, StartingPlayer].


/*  create_bvb_game(-Game, +Bot1Lvl, +Bot2Lvl)

    Creates a BvB (Bot versus Bot) game.
    Game is unified with a list with an initial board, the first player color, the game mode and
    both bots gameplay level (or difficulty).
*/
create_bvb_game(Game, Bot1Lvl, Bot2Lvl):-
    initial_board(Board),
    Game = [Board, black, bvb, Bot1Lvl, Bot2Lvl].


/*  play_game([+Board, +Player|?Other])

    If Board matches the initial board, this is the first move where the piece of starting player
    is placed in the middle. No matter the game mode.
*/
play_game([Board, Player|Other]):-
    initial_board(Board),
    clear_console,
    player_stone(Player, Piece),
    set_cell(9, 9, Piece, Board, NewBoard), 
    switch_turn(Player, NextPlayer),
    print_board(NewBoard),
    print_msg_piece_mid_placed(Player),
    request_enter,
    play_game([NewBoard, NextPlayer|Other]).


/*  play_game([+Board, +Player, pvp])

    PvP game:
        1 - display current Board
        2 - inform that it's the Player's turn
        3 - request the Player's move
        4 - check if current Player won the game
        5 - check if by action of Player, it's enemy won the game
        6 - advance to next turn
*/
play_game([Board, Player, pvp]):-
    player_turn(Board, NewBoard, Player, NextPlayer),
    play_game([NewBoard, NextPlayer, pvp]).


/*  play_game([+Board, +Player, pvb, +Diff, bot])

    Diff is the bot gameplay level (or difficulty).
    Bot plays first.
*/
play_game([Board, Player, pvb, Diff, bot]):-
    bot_turn(Board, NewBoard, Player, NextPlayer, Diff),
    request_enter,
    player_turn(NewBoard, NewBoard2, NextPlayer, _),
    play_game([NewBoard2, Player, pvb, Diff, bot]).


/*  play_game([+Board, +Player, pvb, +Diff, bot])

    Diff is the  bot gameplay level (or difficulty).
    Player plays first.
*/
play_game([Board, Player, pvb, Diff, player]):-
    player_turn(Board, NewBoard, Player, NextPlayer),
    bot_turn(NewBoard, NewBoard2, NextPlayer, _, Diff),
    request_enter,
    play_game([NewBoard2, Player, pvb, Diff, player]).


/*  play_game([+Board, +Player, bvb, +Bot1Lvl, +Bot2Lvl])

    Bot1Lvl is the first bot gameplay level (or difficulty).
    Bot2Lvl is the second bot gameplay level (or difficulty).
    Bot1 plays first.
*/
play_game([Board, Player, bvb, Bot1Lvl, Bot2Lvl]):-
    bot_turn(Board, NewBoard, Player, NextPlayer, Bot1Lvl),
    request_enter,
    bot_turn(NewBoard, NewBoard2, NextPlayer, _, Bot2Lvl),
    request_enter,
    play_game([NewBoard2, Player, bvb, Bot1Lvl, Bot2Lvl]).


/*  game_over(+Board, +Player)

    Checks if Player has won with the current Board.
    Informs players if so.
*/
game_over(Board, Player):-
    check_win(Board, Player),
    clear_console,
    print_board(Board),
    print_msg_win_game(Player),
    request_enter,
    game_mode_menu.

/* Above predicate call failed, player did not win, proceed */
game_over(_, _).


/*  player_turn(+Board, -NewBoard, +Player, -NextPlayer)

    Player turn events:
        1 - display current Board
        2 - inform that it's the Player's turn
        3 - request the Player's move
        4 - check if current Player won the game
        5 - check if by action of Player, it's enemy won the game
        6 - advance to next turn
*/
player_turn(Board, NewBoard, Player, NextPlayer):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    player_move(Board, Player, NewBoard),
    switch_turn(Player, NextPlayer),
    game_over(NewBoard, Player),
    game_over(NewBoard, NextPlayer).


/*  bot_turn(+Board, -NewBoard, +Player, -NextPlayer, +Diff)

    Bot turn events:
        1 - display current Board
        2 - inform that it's the Player's turn
        3 - request the Player's move
        4 - check if current Player won the game
        5 - check if by action of Player, it's enemy won the game
        6 - advance to next turn
*/
bot_turn(Board, NewBoard, Player, NextPlayer, Diff):-
    clear_console,
    print_board(Board),
    print_player_turn(Player),
    bot_move(Diff, Board, Player, NewBoard),
    switch_turn(Player, NextPlayer),
    game_over(NewBoard, Player),
    game_over(NewBoard, NextPlayer).


/*  player_move(+Board, +Player, -NewBoard)

    Requests for the Player move, validates it, executes it and unifies
    NewBoard with the resulting Board.

    Direction can be:
        1 - TOP
        2 - RIGHT
        3 - BOT
        4 - LEFT
*/
player_move(Board, Player, NewBoard):-
    get_direction(Direction),
    get_coord(Coord, Direction),
    valid_move(Board, Coord, Direction),
    player_stone(Player, Piece),
    move(Board, NewBoard, Coord, Direction, Piece).   