% a)
revert(L, InvL):- 
    rev(L, [], InvL).

rev([], R, R).
rev([H|T], S, R):- 
    rev(T, [H|S], R).

palindrome_rev(L):-
    revert(L, L).


% b)
palindrome(L):-
    palindrome_aux(L, [], L2),
    L == L2.

palindrome_aux([], Acc, Acc).
palindrome_aux([H|T], Acc, L2):-
    palindrome_aux(T, [H|Acc], L2).

