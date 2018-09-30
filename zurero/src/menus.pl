%displays main menu options, reads user input and proceeds accordingly
mainMenu:-
	printMainMenu,
	getChar(Input),

    (   
        %menu options
		Input = '1' -> write('menu to select game type (bot or pvp)');
		Input = '2' -> write('instructions');
		Input = '3' -> write('authors, UC, etc etc...');
		Input = '4';

        %no valid option selected
		nl,
		write('Error: invalid input.'), nl,
		pressEnterToContinue, nl,
		mainMenu
	).


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
