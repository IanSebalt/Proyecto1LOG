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

join(Grid, NumOfColumns, Path, [RG, GG, FG]):-
	pathDelete(Grid, Path, NumOfColumns, R, Rest),
	last(Path, L),
	L = [X, Y],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	logbN(R, 2, Log),
	Ins is 2 ** Log,
	replace(Rest, PosIns, Ins, RG),
	NumOfColumn is NumOfColumns - 1,
	gravity(RG, NumOfColumn, NumOfColumns, GG),
	fillZeros(GG, FG).

fillZeros(Grid, Grid) :-
	\+nth0(_, Grid, 0).
fillZeros(Grid, FG) :-
	nth0(P, Grid, 0),
	randomSquare(Grid, Square),
	replace(Grid, P, Square, FPG),
	fillZeros(FPG, FG).

randomSquare(_, Square) :-
	%max_list(Grid, Max),
	%logbN(Max, 2, Ans),
	%F is Ans - 4,
	%T is Ans - 2,
    random_between(1, 5, Random),
    Square is 2 ** Random.
	
gravity(Grid, -1, _, Grid).
gravity(Grid, NumOfColumn, NumOfColumns, GG) :-
	length(Grid, Le),
	NumOfRows is (Le / NumOfColumns) - 1,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gravityOnColumn(Grid, Bag, Bag, NumOfColumns, GB),
	NextColumn is NumOfColumn - 1,
	gravity(GB, NextColumn, NumOfColumns, GG).

gravityOnColumn(Grid, [], GFPath, NumOfColumns, Grid) :-
	gotGravCol(Grid, GFPath, NumOfColumns).
gravityOnColumn(Grid, [], GFPath, NumOfColumns, GCG) :-
	\+gotGravCol(Grid, GFPath, NumOfColumns),
	gravityOnColumn(Grid, GFPath, GFPath, NumOfColumns, GCG).
gravityOnColumn(Grid, GPath, GFPath, NumOfColumns, GCG) :-
	GPath = [[X, _] | GPT],
	X = 0,
	gravityOnColumn(Grid, GPT, GFPath, NumOfColumns, GCG).
gravityOnColumn(Grid, GPath, GFPath, NumOfColumns, GCG) :-
	GPath = [[X, Y] | GPT],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	PosInsPrev is ((X - 1) * NumOfColumns) + (Y mod NumOfColumns),
	X > 0,
	((nth0(PosIns, Grid, 0),
	nth0(PosInsPrev, Grid, 0),
	gravityOnColumn(Grid, GPT, GFPath, NumOfColumns, GCG));
	(nth0(PosIns, Grid, 0),
	nth0(PosInsPrev, Grid, P),
	P > 0,
	replace(Grid, PosIns, P, Z),
	replace(Z, PosInsPrev, 0, GOP),
	gravityOnColumn(GOP, GPT, GFPath, NumOfColumns, GCG));
	(nth0(PosIns, Grid, J),
	J > 0,
	nth0(PosInsPrev, Grid, K),
	K >= 0),
	gravityOnColumn(Grid, GPT, GFPath, NumOfColumns, GCG)).

gotGravCol(_, [], _).
gotGravCol(Grid, Bag, NumOfColumns) :-
	Bag = [[X, _] | BT],
	X = 0,
	gotGravCol(Grid, BT, NumOfColumns).
gotGravCol(Grid, Bag, NumOfColumns) :-
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
	gotGravCol(Grid, BT, NumOfColumns).

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