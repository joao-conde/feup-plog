%displays main menu options, reads user input and proceeds accordingly
main_menu:-
	print_main_menu,
	get_char_nl(Input),
	main_menu_option(Input).

main_menu_option('1'):- write('menu to select game type (bot or pvp)').
main_menu_option('2'):- write('instructions').
main_menu_option('3'):- write('authors, UC, etc etc...').
main_menu_option('4'):- write('EXIT').
main_menu_option(_):- 
	nl,
	write('Error: invalid input.'), nl,
	request_enter, nl,
	main_menu.

%clears screen and displays main menu options
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
