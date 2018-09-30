%Game structure and predicates to operate with it

%http://www.iggamecenter.com/info/en/zurero.html

create_pvp_game(Game):-
    initial_board(Board),
    Game = [Board].

initial_board([]).