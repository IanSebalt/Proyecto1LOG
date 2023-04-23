:- module(proylcc, 
	[  
		join/4
	]).

library(lists).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, [RG]):-
	pathDelete(Grid, Path, NumOfColumns, R, Rest),
	last(Path, L),
	L = [X, Y],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	logbN(R, 2, Log),
	Ins is 2 ** Log,
	replace(Rest, PosIns, Ins, RG).

pathDelete(X, [], _, 0, X).
pathDelete(Grid, Path, NumOfColumns, RP + Sum, Rest) :-
	Path = [[X, Y] | PT],
	Pos is (X * NumOfColumns) + (Y mod NumOfColumns),
	nth0(Pos, Grid, RP),
	replace(Grid, Pos, 0, GR),
	pathDelete(GR, PT, NumOfColumns, Sum, Rest).

logbN(X,_,0) :- X =< 1.
logbN(N,B,Ans):-
    N > 1,
    N1 is N/B,
    logbN(N1, B, A),
    Ans is A + 1.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
	I > 0, I1 is I-1, 
	replace(T, I1, X, R).