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