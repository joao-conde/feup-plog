%%%%%%%%%%%%%%%%%%%%%%%
%    MENUS MODULE     %
%%%%%%%%%%%%%%%%%%%%%%%

/* Main menu options */
main_menu:-
	print_main_menu,
	get_int(Input),
	main_menu_option(Input).

main_menu_option(1):- 
	puzzles_menu(1).

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
	write('=        Houses Puzzles         =\n'),
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
puzzles_menu(Page):-
	print_game_puzzles_list(Page),
	get_int(Input),
	puzzles_menu_option(Input, Page).

puzzles_menu_option(0, 1):-
	puzzle0(Puzzle),
	connect(Puzzle),
	request_enter,
	puzzles_menu(1).

puzzles_menu_option(1, 1):-
	puzzles_menu(1).

puzzles_menu_option(2, 1):-
	puzzles_menu(1).

puzzles_menu_option(3, 1):-
	puzzles_menu(1).

puzzles_menu_option(4, 1):-
	puzzles_menu(1).

puzzles_menu_option(5, 1):-
	puzzles_menu(1).

puzzles_menu_option(6, 1):-
	puzzles_menu(1).

puzzles_menu_option(7, 1):-
	puzzles_menu(1).

puzzles_menu_option(10, 1):-
	puzzles_menu(2).

puzzles_menu_option(11, _):- 
	main_menu.

puzzles_menu_option(_, _):- 
	write('\nError: invalid input.\n'),
	request_enter,
	puzzles_menu(1).

print_game_puzzles_list(1):-
	clear_console,
	write('=================================\n'),
	write('=         Puzzles List          =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   0. Puzzle 0                 =\n'),
	write('=   1. Puzzle 1                 =\n'),
	write('=   2. Puzzle 2                 =\n'),
	write('=   3. Puzzle 3                 =\n'),
	write('=   4. Puzzle 4                 =\n'),
	write('=   5. Puzzle 5                 =\n'),
	write('=   6. Puzzle 6                 =\n'),
	write('=   7. Puzzle 7                 =\n'),
	write('=   8. Puzzle 8                 =\n'),
	write('=   9. Puzzle 9                 =\n'),
	write('=   10. Next Page               =\n'),
	write('=   11. Back to Menu            =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').

print_game_puzzles_list(2):-
	clear_console,
	write('=================================\n'),
	write('=         Puzzles List          =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   0. Puzzle 10                =\n'),
	write('=   1. Puzzle 11                =\n'),
	write('=   2. Puzzle 12                =\n'),
	write('=   3. Puzzle 13                =\n'),
	write('=   4. Puzzle 14                =\n'),
	write('=   5. Puzzle 15                =\n'),
	write('=   6. Puzzle 16                =\n'),
	write('=   7. Puzzle 17                =\n'),
	write('=   11. Back to Menu            =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').
