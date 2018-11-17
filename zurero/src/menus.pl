%%%%%%%%%%%%%%%%%%
%   MAIN MENU    %
%%%%%%%%%%%%%%%%%%
main_menu:-
	print_main_menu,
	get_int(Input),
	main_menu_option(Input).

main_menu_option(1):- game_mode_menu.
main_menu_option(2):- how_to_play.
main_menu_option(3):- about.
main_menu_option(4). %terminates
main_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter, nl,
	main_menu.

print_main_menu:-
	clear_console,
	write('=================================\n'),
	write('=            ZURERO             =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Play                     =\n'),
	write('=   2. How to play              =\n'),
	write('=   3. About                    =\n'),
	write('=   4. Exit                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option: ').


%%%%%%%%%%%%%%%%%%%%%%%
%     HOW TO PLAY     %
%%%%%%%%%%%%%%%%%%%%%%%
how_to_play:-
	print_how_to_play,
	request_enter,
	main_menu.

print_how_to_play:-
	clear_console,
	write('=========================================================================================================\n'),
	write('=                                          HOW TO PLAY                                                  =\n'),
	write('=========================================================================================================\n'),
	write('=                                                                                                       =\n'),
	write('=    The objective of Zurero is to place five stones in a row, horizontally, vertically or diagonally.  =\n'),
	write('=    The first one, who achieves this goal, wins the game.                                              =\n'),
	write('=                                                                                                       =\n'),
	write('=    A game begins with an empty board.                                                                 =\n'),
	write('=    Each player has an allocated color, Black or White.                                                =\n'),
	write('=                                                                                                       =\n'),
	write('=    Black plays first, putting one black stone at the middle of the board.                             =\n'),
	write('=    After this move players take turns alternatively sliding a stone from any edge of the board along  =\n'),
	write('=   the board lines.                                                                                    =\n'),
	write('=                                                                                                       =\n'),
	write('=    A stone slides until it hits another stone already on the board. It\'s not allowed to slide a stone  =\n'),
	write('=   along a line with no other stones on it.                                                            =\n'),
	write('=                                                                                                       =\n'),
	write('=                                                                                                       =\n'),
	write('=    If the stone already on the board that the sliding stone hits has nothing behind it in the sliding =\n'),																                          
	write('=   direction, the stone is pushed backwards one step and the sliding stone moves into the square the   =\n'),
	write('=   preexisting stone previously occupied.                                                              =\n'),
	write('=    If not, the sliding stone stops on impact.                                                         =\n'),
	write('=                                                                                                       =\n'),
	write('=    If by pushing an opponent stone a player places 5 opponent\'s stones in a row then the player      =\n'),   
	write('=   loses the game unless the player forms 5-in-a-row with his own stones at the same move, thus        =\n'),
	write('=   winning the game himself.                                                                           =\n'),
	write('=                                                                                                       =\n'),
	write('=   That\'s it!                                                                                         =\n'),
	write('=   Good luck and have fun!                                                                             =\n'),
	write('=                                                                                                       =\n'),
	write('=========================================================================================================\n').


%%%%%%%%%%%%%%%%%%%%%%%
%        ABOUT        %
%%%%%%%%%%%%%%%%%%%%%%%
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('================================================================\n'),
	write('=                            ABOUT                             =\n'),
	write('================================================================\n'),
	write('=   Zurero is a two-player abstract board game played on a     =\n'),								
	write('=   square board with orthogonal grid lines, with 19x19        =\n'),
	write('=   intersections.                                             =\n'),
	write('=   This game was invented by Jordan Goldstein in 2009.        =\n'),
	write('=                                                              =\n'),
	write('=   Zurero was developed for the course of Logic Programming.  =\n'),
	write('=                                                              =\n'),
	write('=   It is written entirely in SICStus PROLOG                   =\n'),
	write('=                                                              =\n'),
	write('=   Developed by:                                              =\n'),
	write('=    > Afonso Ramos                                            =\n'),
	write('=    > Joao Conde                                              =\n'),
	write('=                                                              =\n'),
	write('================================================================\n').


%%%%%%%%%%%%%%%%%%%%%%%
%   GAME MODE MENU    %
%%%%%%%%%%%%%%%%%%%%%%%
game_mode_menu:-
	print_game_mode_menu,
	get_int(Input),
	game_mode_menu_option(Input).

game_mode_menu_option(1):- 
	create_pvp_game(Game),
	play_game(Game).
	
game_mode_menu_option(2):- player_computer_menu.

game_mode_menu_option(3):-
	bot_select_lvl_menu(Bot1Lvl, '=          Bot1 level?          =\n'),
	bot_select_lvl_menu(Bot2Lvl, '=          Bot2 level?          =\n'),
	create_bvb_game(Game, Bot1Lvl, Bot2Lvl),
	play_game(Game).

game_mode_menu_option(4):- main_menu.
game_mode_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter, nl,
	game_mode_menu.

print_game_mode_menu:-
	clear_console,
	write('=================================\n'),
	write('=           Game Mode           =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Player vs. Player        =\n'),
	write('=   2. Player vs. Computer      =\n'),
	write('=   3. Computer vs. Computer    =\n'),
	write('=   4. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option: ').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Player vs Computer Menu    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
player_computer_menu:-
	print_player_computer_menu,
	get_int(Input),
	player_computer_menu_option(Input).

player_computer_menu_option(1):- 
	starting_player_menu(easy).

player_computer_menu_option(2):- 
	starting_player_menu(hard).

player_computer_menu_option(3):- game_mode_menu. %back to previous menu
player_computer_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter, nl,
	player_computer_menu.

print_player_computer_menu:-
	clear_console,
	write('=================================\n'),
	write('=         Bot Difficulty        =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Easy                     =\n'),
	write('=   2. Hard                     =\n'),
	write('=   3. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Starting Player Selection Menu     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
starting_player_menu(Diff):-
	print_starting_player_menu,
	get_int(Input),
	starting_player_menu_option(Input, Diff).

starting_player_menu_option(1, Diff):-
	create_pvb_game(Game, Diff, player), 
	play_game(Game).

starting_player_menu_option(2, Diff):-
	create_pvb_game(Game, Diff, bot), 
	play_game(Game).

starting_player_menu_option(3, _):-
	player_computer_menu.

starting_player_menu_option(_, _):-
	write('\nError: invalid input.\n'),
	request_enter, nl,
	starting_player_menu.

print_starting_player_menu:-
	clear_console,
	write('=================================\n'),
	write('=         Who plays first?      =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. AI                       =\n'),
	write('=   2. Me                       =\n'),
	write('=   3. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Bot Gameplay Level Selection      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bot_select_lvl_menu(Diff, Message):-
	print_bot_select_lvl_menu(Message),
	get_int(Input),
	bot_select_lvl_menu_option(Input, Diff, Message).

bot_select_lvl_menu_option(1, easy, _).
bot_select_lvl_menu_option(2, hard, _).
bot_select_lvl_menu_option(3, _, _):-
	game_mode_menu.

bot_select_lvl_menu_option(_, Diff, Message):-
	write('\nError: invalid input.\n'),
	request_enter, nl,
	bot_select_lvl_menu(Diff, Message).

print_bot_select_lvl_menu(Message):-
	clear_console,
	write('=================================\n'),
	write(Message),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Easy                     =\n'),
	write('=   2. Hard                     =\n'),
	write('=   3. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').