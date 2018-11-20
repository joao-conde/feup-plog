:-use_module(library(lists)).
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


%7
populationRange(MinPop, MaxPop, Cities):-
	findall(City, (
					city(_, _, City, Inhabitants, _),
					Inhabitants >= MinPop,
					Inhabitants =< MaxPop
				)
				, Cities).


%8
averagePop(District, AveragePop):-
	findall(Inhabitants, (
							district(DistrictID, District),
							city(_, DistrictID, _, Inhabitants, _)
						), Populations),
	length(Populations, Pops),
	sumlist(Populations, Sum),
	AveragePop is Sum / Pops.


%9
distalTwins(Cities, DLA):-
	findall(DLA-City1-City2, 
					(
						computeDLA(City1, City2, DLA)
					), 
					DLAS),
	sort(DLAS, AscDLAS),
	reverse(AscDLAS, DescDLAS),
	processDLAS(DescDLAS, Cities, DLA, [], -1000).

computeDLA(City1ID, City2ID, DLA):-
	relation(City1ID, City2ID, Distance, Inb12, _),
	city(City1ID, _, _, Total, _),
	DLA is Distance * (Inb12 / Total).


processDLAS([], Cities, DLA, Cities, DLA).
processDLAS([HDLA-HC1-HC2|T], Cities, DLA, Acc, CurDLA):-
	HDLA >= CurDLA,
	CurDLA1 is HDLA,
	city(HC1, _, City1Name, _, _),
	city(HC2, _, City2Name, _, _),
	append(Acc, [City1Name-City2Name], Acc1),
	processDLAS(T, Cities, DLA, Acc1, CurDLA1).

processDLAS([HDLA-_-_|T], Cities, DLA, Acc, CurDLA):-
	HDLA < CurDLA,
	processDLAS(T, Cities, DLA, Acc, CurDLA).


%10
districtHasCitiesWithSameExternalWorkers(DistrictID):-
	city(CityID, DistrictID, _, _, _), !,
	( relation(CityID, AnotherCityID, Distance, OutWorkers, OutWorkers) ; 
			relation(AnotherCityID, CityID, Distance, OutWorkers, OutWorkers) ).

/*
	O predicado procura 2 cidades e.g. A e B que tenham o mesmo número de trabalhadores
	a trabalhar na outra cidade i.e. há X cidadãos de A a trabalhar em B e X cidadãos de B
	a trabalhar em A.

	CUT?
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ingrediente(laranjas, 6). 	% existem 6 laranjas
ingrediente(manteigas, 23). % existem 23 manteigas
ingrediente(farinhas, 50). 	% existem 50 farinhas
ingrediente(azeites, 13). 	% existem 13 azeites
ingrediente(nozes, 43). 	% existem 43 nozes
ingrediente(avelas, 55). 	% existem 55 avelas
ingrediente(bolachas, 85). 	% existem 85 bolachas

receita(boloLaranjas, [laranjas/5, avelas/10, nozes/7]).
receita(boloBolacha, [bolachas/20, laranjas/2, manteigas/2, nozes/7]).
receita(boloNozes, [farinhas/20, manteigas/2, nozes/7]).
receita(boloAvelasNozes, [farinhas/20, manteigas/2, avelas/45, nozes/7]).
receita(boloImpossivel, [farinhas/51, manteigas/2, avelas/45, nozes/7]).


%11
podeFazer_Se(Receita):-
	receita(Receita, Ingredientes),
	verificaStock(Ingredientes).

verificaStock([]).
verificaStock([Ing/Quant|T]):-
	ingrediente(Ing, Available),
	Quant =< Available,
	verificaStock(T).


%12
adicionaListasIngredientes([], [], Ingredientes, Ingredientes).

adicionaListasIngredientes([H1|IngList1], [H2|IngList2], Ingredientes, Acc):-
	adicionaIngrediente(H1, Acc, Acc1),
	adicionaIngrediente(H2, Acc1, Acc2),
	adicionaListasIngredientes(IngList1, IngList2, Ingredientes, Acc2).

adicionaListasIngredientes([H1|IngList1], [], Ingredientes, Acc):-
	adicionaIngrediente(H1, Acc, Acc1),
	adicionaListasIngredientes(IngList1, [], Ingredientes, Acc1).

adicionaIngrediente(Ing/Quant, L, [Ing/Quant|L]):-
	\+ member(Ing/_, L).

adicionaIngrediente(Ing/Quant, L, NewList):-
	member(Ing/_, L),
	nth1(_, L, Ing/CurQuant),
	NewQuant is CurQuant + Quant,
	delete(L, Ing/CurQuant, L2),
	append(L2, [Ing/NewQuant], NewList).

cozinhaTodos(Receitas):-
	auxCozinhaTodos(Receitas, Ingredientes, []),
	verificaStock(Ingredientes).

auxCozinhaTodos([], Ingredientes, Ingredientes).
auxCozinhaTodos([Receita|T], Ingredientes, Acc):-
	receita(Receita, ListaIngredientes),
	adicionaListasIngredientes(ListaIngredientes, Acc, Acc1, []),
	auxCozinhaTodos(T, Ingredientes, Acc1).


%13
