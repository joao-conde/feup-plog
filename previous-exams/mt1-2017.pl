:-dynamic(played/4).


%player(Name, UserName, Age).
player('Danny', 'Best Player Ever', 27).
player('Annie', 'Worst Player Ever', 24).
player('Harry', 'A-Star Player', 26).
player('Manny', 'The Player', 14).
player('Jonny', 'A Player', 16).


%game(Name, Categories, MinAge).
game('5 ATG', [action, adventure, open-world, multiplayer], 18).
game('Carrier Shift: Game Over', [action, fps, multiplayer, shooter], 16).
game('Duas Botas', [action, free, strategy, moba], 12).


%played(Player, Game, HoursPlayed, PercentUnlocked)
played('Best Player Ever', '5 ATG', 3, 83).
played('Worst Player Ever', '5 ATG', 52, 9).
played('The Player', 'Carrier Shift: Game Over', 44, 22).
played('A Player', 'Carrier Shift: Game Over', 48, 24).
played('A-Star Player', 'Duas Botas', 37, 16).
played('Best Player Ever', 'Duas Botas', 33, 22).


%1
achievedALot(Player):-
	played(Player, _, _, PerUnlocked),
	PerUnlocked >= 80.


%2
isAgeAppropriate(Name, Game):-
	player(Name, _, Age),
	game(Game, _, MinAge),
	Age >= MinAge.


%3
updatePlayer(Player, Game, Hours, Percentage):-
	retract(played(Player, Game, OldH, OldP)),
	NewH is OldH + Hours,
	NewP is OldP + Percentage,
	assertz(played(Player, Game, NewH, NewP)).


%4
listGamesOfCategory(Cat):-
	game(GameName, Categories, MinAge),
	member(Cat, Categories),
	write(GameName), write(' ('), write(MinAge), write(')\n'),
	fail.

listGamesOfCategory(_).


%5
sumList(List, Sum):-
	sumList(List, Sum, 0).

sumList([], Sum, Sum).
sumList([H|T], Sum, Acc):-
	Acc1 is Acc + H,
	sumList(T, Sum, Acc1).

buildHoursPlayedList(Player, Games, Times):-
	auxBuildHoursPlayedList(Player, Games, Times, []).

auxBuildHoursPlayedList(_, [], Times, Times).
auxBuildHoursPlayedList(Player, [Game|T], Times, Acc):-
	played(Player, Game, Hours, _),
	append([Hours], Acc, Acc1),
	auxBuildHoursPlayedList(Player, T, Times, Acc1).

auxBuildHoursPlayedList(Player, [Game|T], Times, Acc):-
	\+played(Player, Game, _, _),
	append([0], Acc, Acc1),
	auxBuildHoursPlayedList(Player, T, Times, Acc1).

timePlayingGames(Player, Games, ListOfTimes, SumTimes):-
	buildHoursPlayedList(Player, Games, ListOfTimes),
	sumList(ListOfTimes, SumTimes).


%6
bigAchievement(Player, Games):-
	auxBigAchievement(Player, [], Games, []).

auxBigAchievement(Player, CheckedGames, Games, Acc):-
	played(Player, Game, _, PerUnlocked),
	\+ member(Game, CheckedGames),
	PerUnlocked > 80,
	auxBigAchievement(Player, [Game|CheckedGames], Games, [Game|Acc]).

auxBigAchievement(Player, CheckedGames, Games, Acc):-
	played(Player, Game, _, PerUnlocked),
	\+ member(Game, CheckedGames),
	PerUnlocked =< 80,
	auxBigAchievement(Player, [Game|CheckedGames], Games, Acc).


auxBigAchievement(_, _, Games, Games).

	