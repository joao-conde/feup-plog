%Test data
age(maria,30).
age(pedro,25).
age(jose,25).
age(rita,18).

proximity(Person, GoalAge, Diff):-
	age(Person, PersonAge),
	Diff is abs(GoalAge - PersonAge).

getClosest([Prox-Person|T], Prox, ProximityList, Acc):-
	append(Acc, [Person], Acc1), !,
	getClosest(T, Prox, ProximityList, Acc1).

getClosest(_, _, ProximityList, ProximityList).

closestAge(GoalAge, ProximityList):-
	setof(Proximity-Person, proximity(Person, GoalAge, Proximity), [Prox-Pers|T]),
	getClosest([Prox-Pers|T], Prox, ProximityList, []).