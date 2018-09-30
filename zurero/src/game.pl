%Game structure and predicates to operate with it



create_pvp_game(Game):-
    initial_board(Board),
    Game = [Board].

initial_board([]).