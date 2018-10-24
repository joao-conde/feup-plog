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
	write('=================================\n'),
	write('=          HOW TO PLAY          =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   Do stuff                    =\n'),
	write('=   And More stuff              =\n'),
	write('=                               =\n'),
	write('=================================\n').


%%%%%%%%%%%%%%%%%%%%%%%
%        ABOUT        %
%%%%%%%%%%%%%%%%%%%%%%%
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('=================================\n'),
	write('=            ABOUT              =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   By Afonso Jorge Ramos       =\n'),
	write('=   And Joao Conde              =\n'),
	write('=                               =\n'),
	write('=================================\n').


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

game_mode_menu_option(2):- player_computer_menu, game_mode_menu.
game_mode_menu_option(3):- write('LAUNCH BOT V BOT (ez vs hard by default)'), game_mode_menu.
game_mode_menu_option(4):- main_menu.
game_mode_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter, nl,
	game_mode_menu_option.

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

player_computer_menu_option(1):- write('LAUNCH PLAYER VS EASY BOT'), player_computer_menu.
player_computer_menu_option(2):- write('LAUNCH PLAYER VS HARD BOT'), player_computer_menu.
player_computer_menu_option(3). %back to previous menu
player_computer_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter, nl,
	player_computer_menu_option.

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