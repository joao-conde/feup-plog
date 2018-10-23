%Game structure and predicates to operate with it

switch_turn(white, black).
switch_turn(black, white).

create_pvp_game(Game):-
	initial_board(Board),
    Game = [Board, white, pvp].

play_game([Board, Player, pvp]):-
    print_board(Board),
    nl, write('It\'s the '), write(Player), write('\'s player turn'), nl,
    get_piece_pos(Column, Row, 'Insert coords'),
    write('Player '), write(Player), write(' choose '), write(Column), write(' - '), write(Row),nl,
    request_enter,
    switch_turn(Player, NextPlayer),
    play_game([Board, NextPlayer, pvp]).


play_game([Board, Player, pvb]).

play_game([Board, Player, bvb]).
