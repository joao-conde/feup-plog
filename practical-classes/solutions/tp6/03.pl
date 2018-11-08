% Exercício MP3. Idades Mais Próximas
% Implemente utilizando o setof/3, o predicado mais_proximos(+Idade,-ListaProximos) que,
% assumindo a existência de factos idade(Nome,Idade) para representar que um dado indivíduo
% chamado Nome tem idade Idade, devolve em ListaProximos o nome dos indivíduos cuja idade é mais
% próxima de Idade.

% setof(+Template, +Goal, -Set) sorted without dups

%Test facts
idade(maria,30).
idade(pedro,25).
idade(jose,25).
idade(rita,18).

closest(Age, ClosestList). %TODO