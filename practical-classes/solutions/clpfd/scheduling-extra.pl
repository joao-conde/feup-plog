% Task    	Duration        Resources
% T1			16			    2
% T2			6			    9
% T3			13				3
% T4			7				7
% T5			5				10
% T6			18				1
% T7			4				11
% Limit 13

:-use_module(library(clpfd)).

schedule:-
	Tasks = [
		task(S1, 16, E1, 2, T1),
		task(S2, 6, E2, 9, T2),
		task(S3, 13, E3, 3, T3),
		task(S4, 7, E4, 7, T4),
		task(S5, 5, E5, 10, T5),
		task(S6, 18, E6, 1, T6),
		task(S7, 4, E7, 11, T7)
	],
	cumulative(Tasks, [limit(13)]),
	labeling([], Tasks),

	write(Tasks).