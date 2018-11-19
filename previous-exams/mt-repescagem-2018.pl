:-dynamic(relation/5).

%district(District ID, District name).
district(1, 'Porto').
district(2, 'Aveiro').

%city(City ID, District ID, City name, Inhabitants, 
%Inhabitants working in the same city).
city(1, 1, 'Porto', 1000, 800).
city(2, 2, 'Aveiro', 500, 400).
city(3, 1, 'Vila Nova de Gaia', 260, 200).

%relation(city1 ID, city2 ID, distance, inhabitants from city1 working on city
%2, inhabitants from city2 working on city1)
relation(1, 2, 50, 100, 50).
relation(3, 1, 20, 40, 100).
relation(2, 3, 30, 50, 20).


%1
smallTown(City):-
	city(_, _, City, Inhabitants, _),
	Inhabitants < 500.


%2
hasBigCity(District):-
	district(DistrictID, District),
	city(_, DistrictID, _, Inhabitants, _),
	Inhabitants >= 1000.


%3
listCitiesOfDistrict(District):-
	district(DistrictID, District),
	city(_, DistrictID, CityName, Inhabitants, _),
	write(CityName), write(' ('), write(Inhabitants), write(')\n'),
	fail.

listCitiesOfDistrict(_).


%4
transfer(OriginCity, OldCity, NewCity, Amount):-
	retract(relation(OriginCity, OldCity, Dis1, OriginAmount, Origin2)),
	retract(relation(NewCity, OriginCity, Dis2, Dest, DestAmount)),
	UpdateOAmount is OriginAmount - Amount,
	UpdateDestAmount is DestAmount + Amount,
	assertz(relation(OriginCity, OldCity, Dis1, UpdateOAmount, Origin2)),
	assertz(relation(NewCity, OriginCity, Dis2, Dest, UpdateDestAmount)).


%5
sumList(L, Sum):-
	auxSum(L, Sum, 0).

auxSum([], Sum, Sum).
auxSum([H|T], Sum, Acc):-
	Acc1 is Acc + H,
	auxSum(T, Sum, Acc1).

workingOutOfCity(City, OtherCities, ListOfPeople, SumPeople):-
	city(CityID, _, City, _, _),
	processOtherCities(CityID, OtherCities, [], ListOfPeople, []), !,
	sumList(ListOfPeople, SumPeople).

processOtherCities(CityID, [C|Cities], CheckedCities, ListOfPeople, Acc):-
	\+ member(C, Cities),
	city(CID, _, C, _, _),
	relation(CityID, CID, _, In, _),
	append(Acc, [In], Acc1),
	processOtherCities(CityID, Cities, [C|CheckedCities], ListOfPeople, Acc1).

processOtherCities(CityID, [C|Cities], CheckedCities, ListOfPeople, Acc):-
	\+ member(C, Cities),
	city(CID, _, C, _, _),
	relation(CID, CityID, _, _, In),
	append(Acc, [In], Acc1),
	processOtherCities(CityID, Cities, [C|CheckedCities], ListOfPeople, Acc1).


processOtherCities(CityID, [C|Cities], CheckedCities, ListOfPeople, Acc):-
	\+ member(C, Cities),
	append(Acc, [0], Acc1),
	processOtherCities(CityID, Cities, [C|CheckedCities], ListOfPeople, Acc1).


processOtherCities(_, _, _, ListOfPeople, ListOfPeople).


%6
selfEmployer(District, []):-
	\+district(_, District).

selfEmployer(District, Cities):-
	district(DistrictID, District),
	auxSelfEmployer(DistrictID, [], Cities, []).


auxSelfEmployer(DistrictID, CheckedCities, Cities, Acc):-
	city(CityID, DistrictID, CityName, Inhabitants, InCityWorkers),
	\+ member(CityID, CheckedCities),
	Perc is (InCityWorkers * 100) / Inhabitants,
	Perc >= 80,
	append(Acc, [CityName], Acc1),
	auxSelfEmployer(DistrictID, [CityID|CheckedCities], Cities, Acc1), !.

auxSelfEmployer(DistrictID, CheckedCities, Cities, Acc):-
	city(CityID, DistrictID, _, Inhabitants, InCityWorkers),
	\+ member(CityID, CheckedCities),
	Perc is (InCityWorkers * 100) / Inhabitants,
	Perc < 80,
	auxSelfEmployer(DistrictID, [CityID|CheckedCities], Cities, Acc), !.

auxSelfEmployer(_, _, Cities, Cities).


