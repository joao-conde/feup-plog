%-------------------BUBBLESORT-------------------%
bubblesort(List, SortedList):-
    swap(List, List1),
    bubblesort(List1, SortedList).

%list is sorted, predicate swap above fails and so does bubblesort so catch result here 
bubblesort(List, List). 

%swap/2
%swap(L1, L2) either swaps first 2 elements of L1 if first > second and unifies it with L2
%or it does not swap if first =< second and instead recurseively swaps tail elements
swap([X,Y|T], [Y,X|T]):-
    X > Y.

swap([Z|T1], [Z|T2]):- 
    swap(T1, T2).