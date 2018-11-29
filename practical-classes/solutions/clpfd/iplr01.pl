:-use_module(library(clpfd)).

solve_magic_quad_3x3:-

	Vars = [A, B, C, D, E, F, G, H, I],
	domain(Vars, 1, 9),

	all_distinct(Vars),

	%lines
	A + B + C #= S,
	D + E + F #= S,
	G + H + I #= S,

	%columns
	A + D + G #= S,
	B + E + H #= S,
	C + F + I #= S,

	%backslash diagonal
	A + E + I #= S,

	%forwardslash diagonal
	G + E + C #= S,

	labeling([], Vars),
	print_quad(Vars), nl,
	fail.

solve_magic_quad_3x3.


print_quad([A, B, C]):-
	!,
	write([A, B, C]),nl.

print_quad([A, B, C| Vars]):-
	write([A, B, C]),nl,
	print_quad(Vars).