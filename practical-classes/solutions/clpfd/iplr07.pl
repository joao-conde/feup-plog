:-use_module(library(clpfd)).

peru_cost:-
	%Variables and domain restrictions
	FirstDigit in 1..9,
	LastDigit in 0..9,

	Total #= FirstDigit * 1000 + 670 + LastDigit,
	Total #= PeruUnitCost * 72,
	%---------------------------

	labeling([], [FirstDigit, LastDigit, Total, PeruUnitCost]),
	write('Total cost:       '), write(Total), nl,
	write('Cost per turkey:  '), write(PeruUnitCost).