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

join(Grid, NumOfColumns, Path, [RG | GG]):-
	pathDelete(Grid, Path, NumOfColumns, R, Rest),
	last(Path, L),
	L = [X, Y],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	logbN(R, 2, Log),
	Ins is 2 ** Log,
	replace(Rest, PosIns, Ins, RG),
	NumOfColumn is NumOfColumns - 1,
	gravity(RG, NumOfColumn, NumOfColumns, GG).

/**
 * randomSquare(-Square)
 * Square es un cuadrado de dos aleatorio.
*/

randomSquare(Square) :-
    random_between(1, 5, Random),
    Square is 2 ** Random.

/**
 * gravity(+Grid, +NumOfColumn, +NumOfColumns, -GG)
 * GG es la grilla resultante de aplicar la gravedad en la grilla Grid, atravesando las NumOfColumns columnas,
 * manteniendo la columna NumOfColumn a la que actualmente se le está aplicando gravedad. Se empieza por la última columna.
*/

gravity(Grid, -1, NumOfColumns, [Grid]) :-
	NumOfColumn is NumOfColumns - 1,
	gotGravOnAllCols(Grid, NumOfColumn, NumOfColumns).
gravity(Grid, -1, NumOfColumns, [Grid | GT]) :-
	NumOfColumn is NumOfColumns - 1,
	\+gotGravOnAllCols(Grid, NumOfColumn, NumOfColumns),
	gravity(Grid, NumOfColumn, NumOfColumns, GT).
gravity(Grid, NumOfColumn, NumOfColumns, GList) :-
	length(Grid, Le),
	NextColumn is NumOfColumn - 1,
	NumOfRows is (Le / NumOfColumns) - 1,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gravityOnColumn(Grid, Bag, Bag, NumOfColumns, GB),
	gravity(GB, NextColumn, NumOfColumns, GList).

/**
 * gravityOnColumn(+Grid, +GPath, +GFPath, +NumOfColumns, GCG)
 * GCG es la grilla resultanted de aplicar gravedad en la columna indicada por el camino GFPath, siendo GPath lo que resta 
 * recorrer en la columna.
*/

gravityOnColumn(Grid, [], _, _, Grid).
gravityOnColumn(Grid, GPath, GFPath, NumOfColumns, GCG) :-
	GPath = [[X, Y] | GPT],
	PosIns is (X * NumOfColumns) + (Y mod NumOfColumns),
	randomSquare(Square),
	X = 0,
	((nth0(PosIns, Grid, 0),
	replace(Grid, PosIns, Square, GR),
	gravityOnColumn(GR, GPT, GFPath, NumOfColumns, GCG));
	(nth0(PosIns, Grid, P),
	P > 0,
	gravityOnColumn(Grid, GPT, GFPath, NumOfColumns, GCG))).
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

/**
 * gotGravOnAllCols(+Grid, +NumOfColumn, +NumOfColumns)
 * Devuelve verdadero si la lista Grid que representa una grilla de NumOfColumns número de columnas tiene la gravedad
 * aplicada, a partir de la columna NumOfColumn, numerada del 0 al 3, empezando de la columna de mayor coordenada Y hacia atrás.
*/

gotGravOnAllCols(_, -1, _).
gotGravOnAllCols(Grid, NumOfColumn, NumOfColumns) :-
	length(Grid, Le),
	NumOfRows is (Le / NumOfColumns) - 1,
	getAllNaturalsLessThan(NumOfRows, A),
	findall([X, NumOfColumn], member(X, A), Bag),
	gotGravCol(Grid, Bag, NumOfColumns),
	NextCol is NumOfColumn - 1,
	gotGravOnAllCols(Grid, NextCol, NumOfColumns).

/**
 * gotGravCol(+Grid, +Bag, +NumOfColumns)
 * Devuelve true si la columna marcada por Bag tiene la gravedad aplicada, es decir, que no hay ningun 0 entre un bloque
 * de valor mayor y otro.
*/

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

/**
 * pathDelete(+Grid, +Path, +NumOfColumns, +RP, -Rest)
 * Rest es el resultado de dejar en 0 las coordenadas del camino Path dentro de la grilla Grid,
 * siendo RP la sumatoria de los bloques del camino.
*/

pathDelete(X, [], _, 0, X).
pathDelete(Grid, Path, NumOfColumns, RP + Sum, Rest) :-
	Path = [[X, Y] | PT],
	Pos is (X * NumOfColumns) + (Y mod NumOfColumns),
	nth0(Pos, Grid, RP),
	replace(Grid, Pos, 0, GR),
	pathDelete(GR, PT, NumOfColumns, Sum, Rest).

/**
 * pathDeleteIndex(+Grid, +Path, +NumOfColumns, +RP, -Rest)
 * Rest es el resultado de dejar en 0 los índices del camino Path dentro de la grilla Grid,
 * siendo RP la sumatoria de los bloques del camino.
*/

pathDeleteIndex(X, [], _, 0, X).
pathDeleteIndex(Grid, Path, NumOfColumns, RP + Sum, Rest) :-
	Path = [X | PT],
	nth0(X, Grid, RP),
	replace(Grid, X, 0, GR),
	pathDeleteIndex(GR, PT, NumOfColumns, Sum, Rest).

/**
 * collapse(+Grid, +NumOfColumns, -GR)
 * GR es el resultado de colapsar todos los bloques de mismo valor que sean adyacentes
 * dentro de la grilla Grid con cantidad de columnas NumOfColumns.
*/

collapse(Grid, NumOfColumns, GR) :-
	length(Grid, Length),
	getColapsePaths(Grid, NumOfColumns, Length, 0, [], LOP),
	pathsDelete(Grid, LOP, NumOfColumns, GR).

/**
 * pathsDelete(+Grid, +ListOfPaths, +NumOfColumns, -Result)
 * Result es la grilla resultante de reemplazar por 0 todos los caminos de la lista de listas ListOfPaths de la grilla Grid con
 * numero de columnas NumOfColumns.
*/

pathsDelete(Grid, [], NumOfColumns, [Grid | GG]) :-
	NumOfColumn is NumOfColumns - 1,
	gravity(Grid, NumOfColumn, NumOfColumns, GG).
pathsDelete(Grid, ListOfPaths, NumOfColumns, Result) :-
	ListOfPaths = [L | LT],
	pathDeleteIndex(Grid, L, NumOfColumns, R, Rest),
	max_list(L, M),
	logbN(R, 2, Log),
	Ins is 2 ** Log,
	replace(Rest, M, Ins, RG),
	pathsDelete(RG, LT, NumOfColumns, Result).

/**
 * getColapsePaths(+Grid, +NumOfColumns, +GLength, +Pos, +CListOfPaths, -ListOfPaths)
 * ListOfPaths es la lista de listas conteniendo los caminos de posiciones adyacentes de mismo valor de la grilla Grid con longitud GLength
 * y número de columnas GLength, empezando por la posición Pos y la lista de caminos actual CListOfPaths
*/

getColapsePaths(_, _, GLength, Pos, CListOfPaths, CListOfPaths) :-
	Pos == GLength.
getColapsePaths(Grid, NumOfColumns, GLength, Pos, CListOfPaths, ListOfPaths) :-
	NewPos is Pos + 1,
	memberOfListInList(Pos, CListOfPaths),
	getColapsePaths(Grid, NumOfColumns, GLength, NewPos, CListOfPaths, ListOfPaths).
getColapsePaths(Grid, NumOfColumns, GLength, Pos, CListOfPaths, ListOfPaths) :-
	getAdjacent(Grid, [], NumOfColumns, Pos, PathResult),
	NewPos is Pos + 1,
	\+memberOfListInList(Pos, CListOfPaths),
	\+memberOfListInList(Pos, PathResult),
	((length(PathResult, 1),
	getColapsePaths(Grid, NumOfColumns, GLength, NewPos, CListOfPaths, ListOfPaths));
	(length(PathResult, LR),
	LR > 1,
	getColapsePaths(Grid, NumOfColumns, GLength, NewPos, [PathResult | CListOfPaths], ListOfPaths))).

/**
 * memberOfListInList(+X, +ListOfLists)
 * Es verdadero si X está en alguna lista de la lista de listas ListOfLists.
*/

memberOfListInList(X, ListOfLists) :-
	ListOfLists = [L | _],
	member(X, L).
memberOfListInList(X, ListOfLists) :-
	ListOfLists = [L | LT],
	\+member(X, L),
	memberOfListInList(X, LT).

/**
 * getAdjacent(+Grid, +CurrentPath, +NumOfColumns, +Pos, -PathResult)
 * PathResult es el lista de posiciones adyacentes del mismo valor empezando desde
 * la posición Pos en la grilla Grid de cantidad de columnas NumOfColumns, con CurrentPath siendo
 * la lista actual de posiciones adyacentes con el mismo número que Pos.
*/

getAdjacent(Grid, CurrentPath, NumOfColumns, Pos, PathResult) :-
	Pos1 is Pos + 1,
	Pos2 is (Pos - NumOfColumns) + 1,
	Pos3 is Pos - NumOfColumns,
	Pos4 is Pos - NumOfColumns - 1,
	Pos5 is Pos - 1,
	Pos6 is (Pos + NumOfColumns) - 1,
	Pos7 is Pos + NumOfColumns,
	Pos8 is (Pos + NumOfColumns) + 1,
	length(Grid, Length),
	PosList = [Pos1, Pos2, Pos3, Pos4, Pos5, Pos6, Pos7, Pos8],
	findall(PosV, (member(PosV, PosList), validAdjacentPosition(PosV, Pos, NumOfColumns, Length)), VPosList),
	append(CurrentPath, [Pos], CPA),
	nth0(Pos, Grid, Elem),
	checkAdjacent(Grid, CPA, Elem, VPosList, NumOfColumns, PathResult).

/**
 * validAdjacentPosition(+PTV, +Pos, +NumOfColumns, +GLength)
 * Devuelve verdadero si PTV es una posición adyacente válida de Pos en una grilla con número de columnas NumOfColumns y longitud GLength.
*/

validAdjacentPosition(PTV, Pos, NumOfColumns, GLength):-
	PX is PTV mod NumOfColumns,
	PY is div(PTV, NumOfColumns),
	PosX is Pos mod NumOfColumns,
	PosY is div(Pos, NumOfColumns),
	PosXDif is PosX - PX,
	PosYDif is PosY - PY,
	PTV >= 0,
	PTV < GLength,
	-1 =< PosXDif,
	1 >= PosXDif,
	-1 =< PosYDif,
	1 >= PosYDif.

/**
 * checkAdjacent(+Grid, +CurrentPath, +Elem, +PosPath, +NumOfColumns, -PathResult)
 * PathResult es la lista que contiene las posiciones de PosPath que no estén en CurrentPath y tengan
 * de valor a Elem en la grilla Grid con número de columnas NumOfColumns
*/

checkAdjacent(_, CurrentPath, _, [], _, CurrentPath).
checkAdjacent(Grid, CurrentPath, Elem, PosPath, NumOfColumns, PathResult) :-
	PosPath = [X | XT],
	member(X, CurrentPath),
	checkAdjacent(Grid, CurrentPath, Elem, XT, NumOfColumns, PathResult).
checkAdjacent(Grid, CurrentPath, Elem, PosPath, NumOfColumns, PathResult) :-
	PosPath = [X | XT],
	\+nth0(X, Grid, Elem),
	checkAdjacent(Grid, CurrentPath, Elem, XT, NumOfColumns, PathResult).
checkAdjacent(Grid, CurrentPath, Elem, PosPath, NumOfColumns, PathResult) :-
	PosPath = [X | XT],
	X >= 0,
	nth0(X, Grid, Elem),
	\+member(X, CurrentPath),
	getAdjacent(Grid, CurrentPath, NumOfColumns, X, PR),
	checkAdjacent(Grid, PR, Elem, XT, NumOfColumns, PathResult).

/**
 * getAllNaturalsLessThan(+X, -NList)
 * NList es la lista que contiene todos los naturales (incluyendo al 0) menores a X.
*/

getAllNaturalsLessThan(0, [0]).
getAllNaturalsLessThan(X, [X | L]) :-
	X > 0,
	Next is X - 1,
	getAllNaturalsLessThan(Next, L).

/**
 * logbN(+N, +B, -Ans)
 * Ans es el resultado de aplicar logaritmo en base B a N, redondeando hacia el infinito positivo.
*/

logbN(X,_,0) :- X =< 1.
logbN(N,B,Ans):-
    N > 1,
    N1 is N/B,
    logbN(N1, B, A),
    Ans is A + 1.

/**
 * replace(+List, +I, +X, -ListR)
 * ListR es la lista resultante de reemplazar en la lista List el elemento en la posición I por el elemento X.
*/

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
	I > 0, I1 is I-1, 
	replace(T, I1, X, R).