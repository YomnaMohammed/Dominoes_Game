%part1(Depth First Search)

game(N, M, X1, Y1, X2, Y2, Res) :-
    initialize_board(N, M, B),
    addElemToBoard(B, X1, Y1, 1, NB),
    addElemToBoard(NB, X2,Y2, 1, R),
    search([R], [], M, N, Res).

initialize_board(N,M,Board) :-
    length(Board, N),
    initialize_rows(M, Board).

initialize_rows(_, []):-!.
initialize_rows(M, [Row|Rows]) :-
    length(Row, M),
    maplist(=(0), Row), %to initialize this row with 0
    initialize_rows(M, Rows).

addElemToBoard(Board, X, Y, Val, NewBoard) :-
    nth1(X, Board, Row),
    put(Row, Y, Val, NewRow),
    put(Board, X, NewRow, NewBoard).

put([_|T], 1, X, [X|T]):-!.
put([H|T], I, X, [H|Z]) :-
    NI is I - 1,
    put(T, NI, X, Z).


search([],[_|T],_,_,T):-!.

search(Open, Closed, M, N, R):-
	Open = [CurrentNode|RestOfOpen],
	getChildren(CurrentNode, RestOfOpen, Closed, M, N, Children),
	append(RestOfOpen, Children, NewOpen), %BFS
     append(Closed, [CurrentNode], NewClosed),
	search(NewOpen, NewClosed, M, N, R).


getChildren(State, Open, Closed, M, N, Children):-
    findall(NextState, getNextState(State, Open, Closed, M, N, NextState), Children).


getNextState(State, Open, Closed, M, N, NextState):-
	isEmpty(State, RI, CI),
    move(State, RI, CI, M, N, NextState),
	not(member(NextState, Open)),
	not(member(NextState, Closed)).


move(State, RI, CI, M, N, NextState):-
    horizontal(State, RI, CI, M, NextState); vertical(State, RI, CI, N,NextState).

horizontal(B,RI,CI,M,NextState):-
    not(CI = M),
    NCI is CI+1,
    isEmpty(B, RI, NCI),
    addElemToBoard(B, RI, CI, #, NB),
    addElemToBoard(NB, RI, NCI, #, NextState).

vertical(B,RI,CI,N,NextState):-
    not(RI = N),
    NRI is RI+1,
    isEmpty(B, NRI, CI),
    addElemToBoard(B, RI, CI, #, NB),
    addElemToBoard(NB, NRI, CI, #, NextState).

isEmpty(Board, RowIndex, ColIndex) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, 0).





