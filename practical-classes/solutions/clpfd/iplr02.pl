% R1: There are 5 different houses with 5 different colors
% R2: In each house lives one person with unique nacionality, favorite drinks, cigars and pets.
% R3: The English man lives in the red house
% R4: The Spanish man has a dog
% R5: The Norwegian lives on the first house counting from left
% R6: The yellow house owner likes Marlboro cigars
% R7: The man smoking Chesterfields lives next to the man with a pet fox
% R8: The Norwegian lives next to the blue house
% R9: The man that smokes Winston has a pet iguana
% R10: The Lucky Strike smoker drinkrs orange juice
% R11: The Ucranian drinks tea
% R12: The Portuguese smokes SG Lights
% R13: Marbloro is smoken in the house next to the house with a pet horse
% R14: In the green house they drink coffee
% R15: The green house is to the right of the white house
% R16: In the middle house they drink milk
%
%----> Where does the Zebra live and in what house do they drink water?

:-use_module(library(clpfd)).

zebra(Zeb, Agu):-
	% Definição das Variáveis e domínios
	Sol = [Nac,Ani,Beb,Cor,Tab],
	Nac = [Ing, Esp, Nor, Ucr, Por],
	Ani = [Cao, Rap, Igu, Cav, Zeb],
	Beb = [Sum, Cha, Caf, Lei, Agu],
	Cor = [Verm,Verd,Bran,Amar,Azul],
	Tab = [Che, Win, LS, SG, Mar],
	%flatten(Sol,List),
	List=[Ing, Esp, Nor, Ucr, Por, Cao, Rap, Igu, Cav, Zeb, Sum, Cha, Caf,
	Lei, Agu, Verm,Verd,Bran,Amar,Azul,Che, Win, LS, SG, Mar],
	domain(List,1,5),
	% Colocacao das Restrições
	all_different(Nac),
	all_different(Ani),
	all_different(Beb),
	all_different(Cor),
	all_different(Tab),
	Ing #= Verm,
	Esp #= Cao,
	Nor #= 1,
	Amar #= Mar,
	abs(Che-Rap) #= 1,
	abs(Nor-Azul) #= 1,
	Win #= Igu,
	LS #= Sum,
	Ucr #= Cha,
	Por #= SG,
	abs(Mar-Cav) #= 1,
	Verd #= Caf,
	Verd #= Bran+1,
	Lei #= 3,
	% Che #= Rap+1 #\/ Che #= Rap-1
	% Pesquisa da solução
	labeling([],List),
	write(Sol),nl.