% b) Utilizando pesquisa em largura.

% ligado(a,b). ligado(f,i).
% ligado(a,c). ligado(f,j).
% ligado(b,d). ligado(f,k).
% ligado(b,e). ligado(g,l).
% ligado(b,f). ligado(g,m).
% ligado(c,g). ligado(k,n).
% ligado(d,h). ligado(l,o).
% ligado(d,i). ligado(i,f).

ligado(a, b).
ligado(a, e).
ligado(b, a).
ligado(a, c).
ligado(b, d).
ligado(b, e).
ligado(b, f).

% a) recursive and iterative depth-first search
dfs_rec(Src, Src, [Src]).
dfs_rec(Src, Dst, [Src|T]):-
    ligado(Src, A),
    dfs_rec(A, Dst, T).


dfs_iter(Src, Dst, Path):-
    aux_dfs_iter(Src, Dst, Path, [Src]).

aux_dfs_iter(Src, Src, Acc, Acc).
aux_dfs_iter(Src, Dst, Path, Acc):-
    ligado(Src, A),
    \+ member(A, Acc),
    append(Acc, [A], Acc1),
    aux_dfs_iter(A, Dst, Path, Acc1).

best_path(Src, Dst, Sol):-
    setof(Len-Path, (dfs_iter(Src, Dst, Path), length(Path, Len)), [SLen-SPath|_]),
    Sol = SLen-SPath.

% b)
