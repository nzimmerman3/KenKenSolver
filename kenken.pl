

/**
Arguments
Table = list of lists of ints
Val=element to be recieved
**/
increment(Old, New):-
	New is Old + 1.

accessElement(Table, Val, [RowNum|ColNum]):-
	nth(RowNum, Table, Row),
	nth(ColNum, Row, Val).
	%accessRow(Table, RowNum, Row),
	%accessCol(Row, ColNum, Val).

addition(Table, [], 0).
addition(Table, [FirstSquare|RestSquares], Total):-
	accessElement(Table, Val, FirstSquare),
	addition(Table, RestSquares, RestSum),
	Total #= RestSum + Val.

multiplication(Table, [], 1).
multiplication(Table, [FirstSquare|RestSquares], Total):-
	accessElement(Table, Val, FirstSquare),
	multiplication(Table, RestSquares, RestProduct),
	Total #= Val * RestProduct.

subtraction(Table, FirstSquare, SecondSquare, Difference):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	Difference #= First - Second.
subtraction(Table, FirstSquare, SecondSquare, Difference):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	Difference #= Second - First.

division(Table, FirstSquare, SecondSquare, Quotient):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	First / Second #= Quotient.
division(Table, FirstSquare, SecondSquare, Quotient):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	Second / First #= Quotient.


checkSingle(Table, +(Sum, Squares)):-
	addition(Table, Squares, Sum).
checkSingle(Table, *(Product, Squares)):-
	multiplication(Table, Squares, Product).
checkSingle(Table, -(Difference, Square1, Square2)):-
	subtraction(Table, Square1, Square2, Difference).
checkSingle(Table, /(Quotient, Square1, Square2)):-
	division(Table, Square1, Square2, Quotient).


%Checks that each constraint is fulfilled
validRowOps(Table, []).
validRowOps(Table, [First|Rest]):-
	checkSingle(Table, First),
	validRowOps(Table, Rest).

/**
Checks that each Row has the correct length 
and each entry is different and between 1 and N
**/
validRowBasic(Row, N):-
	length(Row, N),
	fd_domain(Row, 1, N),
	fd_all_different(Row).

/**
Tests each Row to make sure they pass validRowBasic
**/
checkEachRowBasic(N, []).
checkEachRowBasic(N, [First|Rest]):-
	validRowBasic(First, N),
	checkEachRowBasic(N, Rest).

/**
GenRow is a list of the entries in the Num Col of the Table
Take the Table and go through each row and 
**/
generateRow(Table, N, ColNum, N, Val):-
	accessElement(Table, Val, [N|ColNum]).
generateRow(Table, RowNum, ColNum, N, [NextVal|RestVal]):-
	accessElement(Table, NextVal, [RowNum|ColNum]),
	increment(RowNum, NewRowNum),
	generateRow(Table, NewRowNum, ColNum, N, RestVal).


/**
Checks that each Col has the correct length, each entry is different and between 1 and N

checkEachColBasic(N, Table, N):-
	generateRow(Table, 1, N, N, GenRow),
	validRowBasic(GenRow, N).
checkEachColBasic(N, Table, NumCol):-
	generateRow(Table, 1, NumCol, N, GenRow),
	validRowBasic(GenRow, N),
	increment(NumCol, NewNumCol),
	checkEachColBasic(N, Table, NewNumCol).
**/

newCheckEachColBasic(N, Table, N):-
	maplist(nth(N), Table, GenRow),
	validRowBasic(GenRow, N).
newCheckEachColBasic(N, Table, CurrentCol):-
	maplist(nth(CurrentCol), Table, GenRow),
	validRowBasic(GenRow, N),
	increment(CurrentCol, NewCurrentCol),
	newCheckEachColBasic(N, Table, NewCurrentCol).

finsh(T):-
	maplist(fd_labeling, T).


/**
starting predicate that takes 
N=number of rows/cols
C=list of constraints
T=table, a list of lists of ints of length N
**/
kenken(N, C, T) :-
	length(T, N),
	checkEachRowBasic(N, T),
	%checkEachColBasic(N, T, 1),
	newCheckEachColBasic(N, T, 1),
	validRowOps(T, C),
	finsh(T).


validRowBasicPlain(Row, N, GoodRow):-
	length(Row, N),
	permutation(Row, GoodRow).

makePlainRow(N, [N], N):-!.
makePlainRow(N, [Curr|Rest], Curr):-
	increment(Curr, NewCurr),
	makePlainRow(N, Rest, NewCurr).

checkEachRowBasicPlain(N, [], GoodRow).
checkEachRowBasicPlain(N, [First|Rest], GoodRow):-
	validRowBasicPlain(First, N, GoodRow),
	checkEachRowBasicPlain(N, Rest, GoodRow).

checkEachColBasicPlain(N, Table, GoodRow, N):-
	maplist(nth(N), Table, GenRow),
	validRowBasicPlain(GenRow, N, GoodRow).
checkEachColBasicPlain(N, Table, GoodRow, CurrentCol):-
	maplist(nth(CurrentCol), Table, GenRow),
	validRowBasicPlain(GenRow, N, GoodRow),
	increment(CurrentCol, NewCurrentCol),
	checkEachColBasicPlain(N, Table, GoodRow, NewCurrentCol).


plainAddition(Table, [], 0).
plainAddition(Table, [FirstSquare|RestSquares], Total):-
	accessElement(Table, Val, FirstSquare),
	plainAddition(Table, RestSquares, RestSum),
	Total is (RestSum + Val).

plainMultiplication(Table, [], 1).
plainMultiplication(Table, [FirstSquare|RestSquares], Total):-
	accessElement(Table, Val, FirstSquare),
	plainMultiplication(Table, RestSquares, RestProduct),
	Total is (Val * RestProduct).

plainSubtraction(Table, FirstSquare, SecondSquare, Difference):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	Difference is (First - Second).
plainSubtraction(Table, FirstSquare, SecondSquare, Difference):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	Difference is (Second - First).

plainDivision(Table, FirstSquare, SecondSquare, Quotient):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	(First / Second) =:= Quotient.
plainDivision(Table, FirstSquare, SecondSquare, Quotient):-
	accessElement(Table, First, FirstSquare),
	accessElement(Table, Second, SecondSquare),
	(Second / First) =:= Quotient.


plainCheckSingle(Table, +(Sum, Squares)):-
	plainAddition(Table, Squares, Sum).
plainCheckSingle(Table, *(Product, Squares)):-
	plainMultiplication(Table, Squares, Product).
plainCheckSingle(Table, -(Difference, Square1, Square2)):-
	plainSubtraction(Table, Square1, Square2, Difference).
plainCheckSingle(Table, /(Quotient, Square1, Square2)):-
	plainDivision(Table, Square1, Square2, Quotient).


%Checks that each constraint is fulfilled
plainValidRowOps(Table, []).
plainValidRowOps(Table, [First|Rest]):-
	plainCheckSingle(Table, First),
	plainValidRowOps(Table, Rest).



plain_kenken(N, C, T):-
	length(T, N),
	makePlainRow(N, GoodRow, 1),
	checkEachRowBasicPlain(N, T, GoodRow),
	checkEachColBasicPlain(N, T, GoodRow, 1),
	plainValidRowOps(T, C).


/**
Trash Predicates



shiftFlip(FlipTable, 0, FlipTable).
shiftFlip(FlipTable, Num, [First|Rest]):-
	shiftFlip(FlipTable, Num - 1, Rest).


addRow([], FlipTable, N).
addRow(CurrRow, FlipTable, Num):-
	shiftFlip(FlipTable, Num, ShiftedFlip),
	addColFlip(CurrRow, ShiftedFlip).


flipTable([FirstRow|Rest], FlipTable, Num):-
	addRow(FirstRow, FlipTable, Num),
	flipTable(Rest, FlipTable, Num + 1).

%finsh([]).
%finish([First|Rest]):-
%	fd_labeling(First),
%	label(Rest).
**/