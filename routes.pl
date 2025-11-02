:- module(routes, [
    default_origin/1,
    route_path/3      % route_path(From, To, Path)
]).

:- use_module(library(lists)).

% Origen por defecto (todos los paquetes salen de aquí)
default_origin('CIUDAD NEILY').

% Grafo dirigido de tramos
edge('CIUDAD NEILY', 'PASO CANOAS').
edge('CIUDAD NEILY', 'RIO CLARO').
edge('RIO CLARO',     'GOLFITO').
edge('PASO CANOAS',   'LA CUESTA').
edge('LA CUESTA',     'LAUREL').
edge('LAUREL',        'NARANJO').

neighbors(Node, Ns) :-
    findall(N, edge(Node, N), Ns).

% BFS para ruta más corta en número de tramos
route_path(From, To, Path) :-
    bfs([[From]], To, Rev),
    reverse(Rev, Path).

bfs([[To|Rest]|_], To, [To|Rest]) :- !.
bfs([Curr|Queue], To, Sol) :-
    Curr = [Head|_],
    neighbors(Head, Nexts),
    exclude({Curr}/[N]>>(memberchk(N, Curr)), Nexts, Fresh),
    maplist({Curr}/[N,NP]>>(NP=[N|Curr]), Fresh, NewPaths),
    append(Queue, NewPaths, Queue1),
    bfs(Queue1, To, Sol).
