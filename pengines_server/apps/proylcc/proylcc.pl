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

join(Grid, NumOfColumns, Path, [RG, GG]):-
	pathDelete(Grid, Path, NumOfColumns, R, Rest),
	last(Path, L),
	L = [X, Y],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	logbN(R, 2, Log),
	Ins is 2 ** Log,
	replace(Rest, PosIns, Ins, RG),
	NumOfColumn is NumOfColumns - 1,
	gravity(RG, NumOfColumn, NumOfColumns, GG).

gravity(Grid, -1, NumOfColumns, Grid) :-
	length(Grid, Le),
	NumOfColumn is NumOfColumns - 1,
	NumOfRows is NumOfColumns / Le,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gotGrav(Grid, Bag, NumOfColumns).
gravity(Grid, -1, NumOfColumns, GPG) :-
	length(Grid, Le),
	NumOfColumn is NumOfColumns - 1,
	NumOfRows is NumOfColumns / Le,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	\+gotGrav(Grid, Bag, NumOfColumns),
	gravity(Grid, NumOfColumn, NumOfColumns, GPG).
gravity(Grid, NumOfColumn, NumOfColumns, GG) :-
	length(Grid, Le),
	NumOfRows is (Le / NumOfColumns) - 1,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gravityOnColumn(Grid, Bag, NumOfColumns, GB),
	NextColumn is NumOfColumn - 1,
	gravity(GB, NextColumn, NumOfColumns, GG).

gravityOnColumn(Grid, [], _, Grid).
gravityOnColumn(Grid, GPath, NumOfColumns, GCG) :-
	GPath = [[X, _] | GPT],
	X = 0,
	gravityOnColumn(Grid, GPT, NumOfColumns, GCG).
gravityOnColumn(Grid, GPath, NumOfColumns, GCG) :-
	GPath = [[X, Y] | GPT],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	PosInsPrev is ((X - 1) * NumOfColumns) + (Y mod NumOfColumns),
	X > 0,
	((nth0(PosIns, Grid, 0),
	nth0(PosInsPrev, Grid, 0),
	gravityOnColumn(Grid, GPT, NumOfColumns, GCG));
	(nth0(PosIns, Grid, 0),
	nth0(PosInsPrev, Grid, P),
	P > 0,
	replace(Grid, PosIns, P, Z),
	replace(Z, PosInsPrev, 0, GOP),
	gravityOnColumn(GOP, GPT, NumOfColumns, GCG));
	(nth0(PosIns, Grid, J),
	J > 0,
	nth0(PosInsPrev, Grid, K),
	K >= 0),
	gravityOnColumn(Grid, GPT, NumOfColumns, GCG)).

gotGrav(_, -1, _).
gotGrav(Grid, NumOfColumn, NumOfColumns) :-
	length(Grid, Le),
	NumOfColumn is NumOfColumns - 1,
	NumOfRows is NumOfColumns / Le,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gotGravCol(Grid, Bag).

gotGravCol(_, []).
gotGravCol(Grid, Bag) :-
	Bag = [[X, _] | BT],
	X = 0,
	gotGravCol(Grid, BT).
gotGravCol(Grid, Bag) :-
	Bag = [[X, Y] | BT],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	PosInsPrev is ((X - 1) * NumOfColumns) + (Y mod NumOfColumns),
	X > 0,
	((nth0(PosIns, Grid, 0),
	nth0(PosInsPrev, Grid, 0));
	(nth0(PosIns, Grid, J),
	J > 0,
	nth0(PosInsPrev, Grid, K),
	K >= 0)),
	gotGravCol(Grid, BT).

pathDelete(X, [], _, 0, X).
pathDelete(Grid, Path, NumOfColumns, RP + Sum, Rest) :-
	Path = [[X, Y] | PT],
	Pos is (X * NumOfColumns) + (Y mod NumOfColumns),
	nth0(Pos, Grid, RP),
	replace(Grid, Pos, 0, GR),
	pathDelete(GR, PT, NumOfColumns, Sum, Rest).

getAllNaturalsLessThan(0, [0]).
getAllNaturalsLessThan(X, [X | L]) :-
	X > 0,
	Next is X - 1,
	getAllNaturalsLessThan(Next, L).

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