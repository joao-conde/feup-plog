% b) Utilizando pesquisa em aux_bfs.

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
ligado(e, f).

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

/*
 Extends the path queue up to a child N1 of parent N, checking if N1 does not belong to the path queue
 thus preventing cycles
*/
find_unchecked_nodes([N|Path], [N1,N|Path]):-
    ligado(N, N1),
    \+ member(N1, Path).

/* Finds the Path between Start and Finish */
bfs(Start, Finish, Path):-
    aux_bfs([[Start]], Finish, Sol1),
    reverse(Sol1, Path).

/* Breadth-first search */
aux_bfs([[Finish|T]|_], Finish, [Finish|T]).
aux_bfs([T|Queue], Finish, Sol):-
    findall(ChildNodes,
                find_unchecked_nodes(T, ChildNodes),
                Nodes),
    append(Queue, Nodes, ExtendedQueue),
    aux_bfs(ExtendedQueue, Finish, Sol).

reverse(L, L2):-
    aux_rev(L, L2, []).

aux_rev([], Acc, Acc).
aux_rev([H|T], L2, Acc):-
    aux_rev(T, L2, [H|Acc]).

    
