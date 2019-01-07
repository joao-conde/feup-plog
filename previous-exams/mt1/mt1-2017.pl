:- dynamic(played/4).
:- use_module(library(lists)).


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
	sumlist(ListOfTimes, SumTimes).


%6
fewHours(Player, Games) :-
    countHours(Player, Games, [], []).

countHours(Player, Games, Acc, OldGames) :-
    played(Player, GameTemp, HoursTemp, _),
    \+member(GameTemp, OldGames),
    append([GameTemp], OldGames, NewOldGames),
    HoursTemp < 10,
    append([GameTemp], Acc, Acc1),
    countHours(Player, Games, Acc1, NewOldGames), !.

countHours(_, Games, Games, _).


%6
bigAchievement(Player, Games):-
	auxBigAchievement(Player, [], Games, []).

auxBigAchievement(Player, CheckedGames, Games, Acc):-
	played(Player, Game, _, PerUnlocked),
	\+ member(Game, CheckedGames),
	PerUnlocked > 80, !,
	auxBigAchievement(Player, [Game|CheckedGames], Games, [Game|Acc]).

auxBigAchievement(Player, CheckedGames, Games, Acc):-
	played(Player, Game, _, PerUnlocked),
	\+ member(Game, CheckedGames),
	PerUnlocked =< 80, !,
	auxBigAchievement(Player, [Game|CheckedGames], Games, Acc).


auxBigAchievement(_, _, Games, Games).


%7
ageRange(MinAge, MaxAge, Players):-
	findall(Player, (player(Player, _, Age), 
					Age >= MinAge, Age =< MaxAge), 
					Players).


%8
averageAge(Game, AverageAge):-
	findall(Age, (played(Player, Game, _, _),
					player(_, Player, Age)), PlayersAge),
	sumlist(PlayersAge, SumAges),
	length(PlayersAge, NumAges),
	AverageAge is SumAges / NumAges.


%9
mostEffectivePlayers(Game, Players):-
	findall(Ratio-Player, (
					played(Player, Game, Hours, Perc),
					Ratio is Perc / Hours
					), 
					PlayersRatios),
    sort(PlayersRatios, SortedPlayersRatios), 
    /* 
        sorts to prevent the case of a player having the biggest ratio so far
        and then another showing up with an even bigger ratio 
    */
    reverse(SortedPlayersRatios, NewPlayersRatios),
	bestPlayers(NewPlayersRatios, Players, [], -1).

bestPlayers([], Players, Players, _).
bestPlayers([R-P|T], Player, Acc, CurRatio):-
	CurRatio =< R,
	CurRatio1 is R,
	append(Acc, [P], Acc1),
	bestPlayers(T, Player, Acc1, CurRatio1).

bestPlayers([R-_|T], Player, Acc, CurRatio):-
	CurRatio > R,
	bestPlayers(T, Player, Acc, CurRatio).


%10
checkMinAge(PlayerUserName):-
	player(_, PlayerUserName, Age), !,
	\+ ( played(PlayerUserName, Game, _, _),
		game(Game, _, MinAge),
	 	MinAge > Age ).	

/*
	O predicado verifica se o jogador PlayerUserName joga algum jogo
	para o qual não tenha a idade mínima.

	

*/


%11 - not certain
distance(1, 0, 8).
distance(2, 0, 8).
distance(3, 0, 7).
distance(4, 0, 7).

distance(3, 2, 2).
distance(4, 2, 4).
distance(5, 2, 4).

distance(3, 4, 3).
distance(3, 5, 3).

distance(4, 5, 1).

distance(X, X, 0).
distance(A, B, C):-
	distance(B, A, C).


%13
tree(a, b, c).
tree(b, d, f).
tree(c, void, void).
tree(d, void, void).
tree(f, void, void).

printTree(void).
printTree(Root):-
	write(Root), nl,
	tree(Root, L, R),
	printTree(L), printTree(R).