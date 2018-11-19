%participant(Id,Age,Performance)
participant(1234, 17, 'Pe coxinho').
participant(3423, 21, 'Programar com os pes').
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
	member(120, Performance).
	
	
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
	
	
%3
patientJuri(JuriMember):-
	performance(X, PerformanceX),
	performance(Y, PerformanceY),
	X \= Y,
	getElement(JuriMember, PerformanceX, 120),
	getElement(JuriMember, PerformanceY, 120).


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


%5
allPerfs:-
	participant(Participant, _, Show),
	performance(Participant, Times),
	write(Participant), write(':'), write(Show), write(':'), write(Times), nl,
	fail.

allPerfs.


%6
successfulParticipant([]).
successfulParticipant([120|T]):-
	successfulParticipant(T).

nSuccessfulParticipants(T):-
	findall(P, 
			(performance(P, Times), successfulParticipant(Times)),
			L),
	length(L, T).


%7
listJuriMembersApproval(PTimes, JuriList):-
	auxListJuriMembersApproval(PTimes, JuriList, [], 1).

auxListJuriMembersApproval([], JuriList, JuriList, _).

auxListJuriMembersApproval([120|T], JuriList, Acc, Cnt):-
	append(Acc, [Cnt], Acc1),
	Cnt1 is Cnt + 1,
	auxListJuriMembersApproval(T, JuriList, Acc1, Cnt1).

auxListJuriMembersApproval([PTime|T], JuriList, Acc, Cnt):-
	PTime < 120,
	Cnt1 is Cnt + 1,
	auxListJuriMembersApproval(T, JuriList, Acc, Cnt1).

juriFans(JuriFansList):-
	findall(P-X, 
			(	
				performance(P, Times),
				listJuriMembersApproval(Times, X)
			), 
			JuriFansList).


%8
:- use_module(library(lists)).

eligibleOutcome(Id,Perf,TT) :-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).

nextPhase(N, Participants):-
	setof(TT-Id-Perf, (eligibleOutcome(Id, Perf, TT)), RevPart),
	reverse(RevPart, FullParticipants),
	prefix_length(FullParticipants, Participants, N).


%9
predX(Q,[R|Rs],[P|Ps]) :-
    participant(R,I,P), I=<Q, !,
    predX(Q,Rs,Ps).
predX(Q,[R|Rs],Ps) :-
    participant(R,I,_), I>Q,
    predX(Q,Rs,Ps).
predX(_,[],[]).

/* 
	Para uma idade Q e uma lista de participantes R devolve uma lista de atuações
	realizadas por um participante com idade inferior ou igual a Q.
	Cut verde? idk, para este caso dá igual mas para outros poderia nao dar?
*/


%10
impoe(X,L) :-
    length(Mid,X),
    append(L1,[X|_],L), 
	append(_,[X|Mid],L1).

/*
	Garante que na lista L entre cada par de elementos X
	existem X elementos.
*/


%11
langford(3, [3,1,2,1,3,2]).
langford(3, [2,3,1,2,1,3]).
langford(N, L):-
	N2 is 2*N,
	length(L, N2),
	impoe(N, L).