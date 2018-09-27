%=================%
%= @@ game menus =%
%=================%
mainMenu:-
	printMainMenu,
	getChar(Input),
	(
		Input = '1' -> gameModeMenu, mainMenu;
		Input = '2' -> helpMenu, mainMenu;
		Input = '3' -> splashScreen;
		Input = '4';

		nl,	write('Error: invalid input.'), nl,
		pressEnterToContinue, nl, 
		mainMenu
	).

splashScreen:-
	clearScreen,
	write('==============================================================================================================='), nl,
	write('=.______          ___       ______  __  .__   __.   _______     __  ___  __  .__   __.   _______      _______.='), nl,
	write('=|   _  \\        /   \\     /      ||  | |  \\ |  |  /  _____|   |  |/  / |  | |  \\ |  |  /  _____|    /       |='), nl,
	write('=|  |_)  |      /  ^  \\   |  ,----\'|  | |   \\|  | |  |  __     |  \'  /  |  | |   \\|  | |  |  __     |   (----\`='), nl,
	write('=|      /      /  /_\\  \\  |  |     |  | |  . \`  | |  | |_ |    |    <   |  | |  . \`  | |  | |_ |     \\   \\    ='), nl,
	write('=|  |\\  \\----./  _____  \\ |  \`----.|  | |  |\\   | |  |__| |    |  .  \\  |  | |  |\\   | |  |__| | .----)   |   ='), nl,
	write('=| _| \`._____/__/     \\__\\ \\______||__| |__| \\__|  \\______|    |__|\\__\\ |__| |__| \\__|  \\______| |_______/    ='), nl,
	write('==============================================================================================================='), nl,
	write('=                                                                                                             ='), nl,
	write('=                                                                                                             ='), nl,
	write('=   Developed by:                                                                                             ='), nl,
	write('=    > Afonso Jorge Ramos                                                                                     ='), nl,
	write('=    > Joao Conde                                                                                             ='), nl,
	write('=                                                                                                             ='), nl,
	write('==============================================================================================================='), nl,
	pressEnterToContinue, mainMenu.

printMainMenu:-
	clearScreen,
	write('=================================='), nl,
	write('=     ..:: Racing Kings ::..     ='), nl,
	write('=================================='), nl,
	write('=                                ='), nl,
	write('=   1. Play                      ='), nl,
	write('=   2. How to play               ='), nl,
	write('=   3. Splash Art                ='), nl,
	write('=   4. Exit                      ='), nl,
	write('=                                ='), nl,
	write('=================================='), nl,
	write('Choose an option:'), nl.

gameModeMenu:-
	printgameModeMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVPlayer;
		Input = '2' -> clearScreen, difficultyMenu;
		Input = '3' -> clearScreen, botVBot;
		Input = '4';

		nl,
		write('Error: invalid input.'), nl,
		pressEnterToContinue, nl,
		gameModeMenu
	).

difficultyMenu:-
	printDifficultyMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVBot;
		Input = '2' -> clearScreen, playerVBotHard;
		Input = '3';

		nl,
		write('Error: invalid input.'), nl,
		pressEnterToContinue, nl,
		difficultyMenu
	).

printDifficultyMenu:-
	clearScreen,
	write('================================='), nl,
	write('=    ..:: Bot Difficulty ::..   ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Bimbi                    ='), nl,
	write('=   2. Optimus Prime            ='), nl,
	write('=   3. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

printgameModeMenu:-
	clearScreen,
	write('================================='), nl,
	write('=      ..:: Game Mode ::..      ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Player vs. Player        ='), nl,
	write('=   2. Player vs. Computer      ='), nl,
	write('=   3. Computer vs. Computer    ='), nl,
	write('=   4. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

helpMenu:-
	clearScreen,
	write('============================================================================'), nl,
	write('=                      ..:: How to play ::..                               ='), nl,
	write('============================================================================'), nl,
	write('=                                                                          ='), nl,
	write('= White pieces start.                                                      ='), nl,
	write('=                                                                          ='), nl,
	write('= The objective is to move your king in order to reach the last row first. ='), nl,
	write('=                                                                          ='), nl,
	write('= No move can place your or the enemy\'s king in check.                    ='), nl,
	write('=                                                                          ='), nl,
	write('= If the White king reaches the last row first, the Black king has one     ='), nl,
	write('= extra move to attempt to reach this last row in order to tie the match.  ='), nl,
	write('=                                                                          ='), nl,
	write('= All other rules related to each piece are the same as normal chess.      ='), nl,
	write('=                                                                          ='), nl,
	write('============================================================================'), nl,
	pressEnterToContinue, nl.
