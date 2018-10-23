%%%%%%%%%%%%%%%%%%
%   MAIN MENU    %
%%%%%%%%%%%%%%%%%%
main_menu:-
	print_main_menu,
	get_char_nl(Input),
	main_menu_option(Input).

main_menu_option('1'):- game_mode_menu.
main_menu_option('2'):- how_to_play.
main_menu_option('3'):- about.
main_menu_option('4'). %terminates
main_menu_option(_):- 
	nl,
	write('Error: invalid input.'), nl,
	request_enter, nl,
	main_menu.

print_main_menu:-
	clear_console,
	write('================================='), nl,
	write('=            ZURERO             ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Play                     ='), nl,
	write('=   2. How to play              ='), nl,
	write('=   3. About                    ='), nl,
	write('=   4. Exit                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.


%%%%%%%%%%%%%%%%%%%%%%%
%     HOW TO PLAY     %
%%%%%%%%%%%%%%%%%%%%%%%
how_to_play:-
	print_how_to_play,
	request_enter,
	main_menu.

print_how_to_play:-
	clear_console,
	write('================================='), nl,
	write('=          HOW TO PLAY          ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   Do stuff                    ='), nl,
	write('=   And More stuff              ='), nl,
	write('=                               ='), nl,
	write('================================='), nl.


%%%%%%%%%%%%%%%%%%%%%%%
%        ABOUT        %
%%%%%%%%%%%%%%%%%%%%%%%
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('================================='), nl,
	write('=            ABOUT              ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   By Afonso Jorge Ramos       ='), nl,
	write('=   And Joao Conde              ='), nl,
	write('=                               ='), nl,
	write('================================='), nl.


%%%%%%%%%%%%%%%%%%%%%%%
%   GAME MODE MENU    %
%%%%%%%%%%%%%%%%%%%%%%%
game_mode_menu:-
	print_game_mode_menu,
	get_char_nl(Input),
	game_mode_menu_option(Input).

game_mode_menu_option('1'):- write('LAUNCH PVP GAME'), game_mode_menu.
game_mode_menu_option('2'):- player_computer_menu, game_mode_menu.
game_mode_menu_option('3'):- write('LAUNCH BOT V BOT (ez vs hard by default)'), game_mode_menu.
game_mode_menu_option('4'):- main_menu.
game_mode_menu_option(_):- 
	nl,
	write('Error: invalid input.'), nl,
	request_enter, nl,
	game_mode_menu_option.

print_game_mode_menu:-
	clear_console,
	write('================================='), nl,
	write('=           Game Mode           ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Player vs. Player        ='), nl,
	write('=   2. Player vs. Computer      ='), nl,
	write('=   3. Computer vs. Computer    ='), nl,
	write('=   4. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Player vs Computer Menu    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
player_computer_menu:-
	print_player_computer_menu,
	get_char_nl(Input),
	player_computer_menu_option(Input).

player_computer_menu_option('1'):- write('LAUNCH PLAYER VS EASY BOT'), player_computer_menu.
player_computer_menu_option('2'):- write('LAUNCH PLAYER VS HARD BOT'), player_computer_menu.
player_computer_menu_option('3'). %back to previous menu
player_computer_menu_option(_):- 
	nl,
	write('Error: invalid input.'), nl,
	request_enter, nl,
	player_computer_menu_option.

print_player_computer_menu:-
	clear_console,
	write('================================='), nl,
	write('=         Bot Difficulty        ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Easy                     ='), nl,
	write('=   2. Hard                     ='), nl,
	write('=   3. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.