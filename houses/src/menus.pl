%%%%%%%%%%%%%%%%%%%%%%%
%    MENUS MODULE     %
%%%%%%%%%%%%%%%%%%%%%%%

/* Main menu options */
main_menu:-
	print_main_menu,
	get_int(Input),
	main_menu_option(Input).

main_menu_option(1):- 
	game_mode_menu.

main_menu_option(2):- 
	how_to_play.

main_menu_option(3):- 
	about.

/* Does nothing, terminates program */
main_menu_option(4).

main_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter,
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
	write('Choose an option:\t').


/* How to play display */
how_to_play:-
	print_how_to_play,
	request_enter,
	main_menu.

print_how_to_play:-
	clear_console,
	write('=========================================================================================================\n'),
	write('=                                       Houses Puzzles                                                  =\n'),
	write('=========================================================================================================\n'),
	write('=                                                                                                       =\n'),
	write('=                                                                                                       =\n'),
	write('=   Good luck and have fun!                                                                             =\n'),
	write('=                                                                                                       =\n'),
	write('=========================================================================================================\n').


/* About display */
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('================================================================\n'),
	write('=                            ABOUT                             =\n'),
	write('================================================================\n'),
	write('=   In Houses Puzzles the objective is to connect pairs of     =\n'),								
	write('=   houses so that there are only two different distances.     =\n'),
	write('=   Each puzzle below has a unique solution.                   =\n'),
	write('=   All pre-loaded puzzles were created by Erich               =\n'),
	write('=   Friedman, 2008.                                            =\n'),
	write('=                                                              =\n'),
	write('=   Houses Puzzles was developed for the course of             =\n'),
	write('=   Logic Programming.                                         =\n'),
	write('=   It is written entirely in SICStus PROLOG                   =\n'),
	write('=                                                              =\n'),
	write('=   Developed by:                                              =\n'),
	write('=    > Afonso Ramos                                            =\n'),
	write('=    > Joao Conde                                              =\n'),
	write('=                                                              =\n'),
	write('================================================================\n').


/* Game mode menu options */
game_mode_menu:-
	print_game_puzzles_list,
	get_int(Input),
	game_mode_menu_option(Input).

game_mode_menu_option(1):.
	
game_mode_menu_option(2).

game_mode_menu_option(4):- 
	main_menu.

game_mode_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter,
	game_mode_menu.

print_game_puzzles_list:-
	clear_console,
	write('=================================\n'),
	write('=           Game Mode           =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   0. Puzzle 0                 =\n'),
	write('=   1. Puzzle 1                 =\n'),
	write('=   2. Puzzle 2                 =\n'),
	write('=   3. Puzzle 3                 =\n'),
	write('=   4. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').
