%participant(Id,Age,Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').

%performance(Id,Times)
performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).


%1
madeItThrough(Participant):-
	performance(Participant, Performance),
	auxMadeItThrough(Performance).
	
auxMadeItThrough([Score|T]):-
	Score < 120,
	auxMadeItThrough(T).
	
auxMadeItThrough([Score|_]):-
	Score = 120.
	
	
%2
getElement(1, [Element|_], Element).
getElement(Idx1Based, [_|T], Element):-
	Idx1BasedUpdate is Idx1Based - 1,
	getElement(Idx1BasedUpdate, T, Element).
	
juriTimes(Participants, JuriMember, Times, Total):-
	auxJuriTimes(Participants, JuriMember, Times, Total, [], 0).

auxJuriTimes([], _, Times, Total, Times, Total).	
auxJuriTimes([Participant|T], JuriMember, Times, Total, AccTimes, AccTotal):-
	performance(Participant, Performance),
	getElement(JuriMember, Performance, Time),
	append(AccTimes, [Time], AccTimes2),
	AccTotal2 is AccTotal + Time,
	auxJuriTimes(T, JuriMember, Times, Total, AccTimes2, AccTotal2).
	
	
/*
%3
patientJuri(JuriMember):-
	buildTimesList(JuriMember, [], JuriTimes, []), write(T, JuriTimes).
	
buildTimesList(JuriMember, Participants, JuriTimes, Acc):-
	performance(Participant, Times),
	\+ member(Participant, Participants),
	getElement(JuriMember, Times, JuriTime),
	buildTimesList(JuriMember, [Participant|T], JuriTimes, [JuriTime|Acc]).
	
buildTimesList(_, _, JuriTimes, JuriTimes).*/


%4
bestParticipant(P1, P2, P):-
	performance(P1, Times1),
	performance(P2, Times2),
	sumList(Times1, P1Total),
	sumList(Times2, P2Total),
	P2Total > P1Total,
	P = P2.

bestParticipant(P1, P2, P):-
	performance(P1, Times1),
	performance(P2, Times2),
	sumList(Times1, P1Total),
	sumList(Times2, P2Total),
	P1Total > P2Total,
	P = P1.
	
sumList(List, Sum):-
	auxSumList(List, Sum, 0).
auxSumList([], Sum, Sum).
auxSumList([H|T], Sum, Acc):-
	Acc1 is Acc + H,
	auxSumList(T, Sum, Acc1).
	
	
 
	