aluno(joao, paradigmas).
aluno(maria, paradigmas).
aluno(joel, lab2).
aluno(joel, estruturas).

frequenta(joao, feup).
frequenta(maria, feup).
frequenta(joel, ist).

professor(carlos, paradigmas).
professor(ana_paula, estruturas).
professor(pedro, lab2).

funcionario(pedro, ist).
funcionario(ana_paula, feup).
funcionario(carlos, feup).

% a)
% X é professor de Y
alunos(X, Y):-
    professor(X, Domain),
    aluno(Y, Domain),
    pertence_univ(Y, F),
    pertence_univ(X, F).


% b)
% X é da universidade Y
pertence_univ(X, Y):- funcionario(X, Y).
pertence_univ(X, Y):- frequenta(X, Y).

% c)
/* X é colega de Y

    Se X e Y são alunos, são colegas se forem colegas de disciplina ou de universidade.
    Se X e Y são professores, são colegas se forem da mesma universidade.

*/
colega(X, Y):-
    frequenta(X, F), frequenta(Y, F),
    X \= Y.

colega(X, Y):-
    funcionario(X, F), funcionario(Y, F),
    X \= Y.
