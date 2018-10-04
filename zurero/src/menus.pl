%displays main menu options, reads user input and proceeds accordingly
mainMenu:-
	printMainMenu,
	getChar(Input),
	mainMenuOption(Input).

mainMenuOption('1'):- write('menu to select game type (bot or pvp)').
mainMenuOption('2'):- write('instructions').
mainMenuOption('3'):- write('authors, UC, etc etc...').
mainMenuOption('4'):- write('EXIT').
mainMenuOption(_):- 
	nl,
	write('Error: invalid input.'), nl,
	pressEnterToContinue, nl,
	mainMenu.

%clears screen and displays main menu options
printMainMenu:-
	clearConsole,
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
