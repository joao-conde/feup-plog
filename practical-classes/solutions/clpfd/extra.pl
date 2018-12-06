/*
	12 cars:
		- 4 yellows
		- 2 greens
		- 3 reds
		- 3 blues

	R1: there are 4 colors for 12 cars
	R2: first and last car are of the same color 
	R3: second and the last but one are of the same color 
	R4: the fifth car is blue 
	R5: each sequence of 3 cars have different colors 
	R6: the sequence Yellow-Green-Red-Blue appears exactly once
*/

:-use_module(library(clpfd)).


/*
	Blue - 1
	Yellow - 2
	Green - 3
	Red - 4
*/
solve:-
	length(Colors, 12), domain(Colors, 1, 4),

	%R1
	global_cardinality(Colors, [1-3, 2-4, 3-2, 4-3]),

	%R2
	element(1, Colors, C1), element(12, Colors, C1),

	%R3
	element(2, Colors, C2), element(11, Colors, C2),

	%R4
	element(5, Colors, 1),

	%R5
	trios(Colors),

	%R6
	quad(Colors, 1),
	
	labeling([], Colors),
	write('Queue-->'), write(Colors), nl, fail.

solve.


trios([_, _]).
trios([C1, C2, C3|T]):-
	all_distinct([C1, C2, C3]),
	trios([C2, C3|T]).


quad([_, _, _], 0).
quad([C1, C2, C3, C4|T], Count):-
	quad([C2, C3, C4|T], Count2),
	(C1 #= 2 #/\ C2 #= 3 #/\ C3 #= 4 #/\ C4 #= 1) #<=> Bin,
	Count #= Count2 + Bin.
