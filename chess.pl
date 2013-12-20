

square([X,Y]) :-
    file(X), rank(Y).

file(X) :- between(0,7,X).
file([X,_], X).

rank(X) :- between(0,7,X).
rank([_,Y], Y).

squareCoord(S, X) :- file(S, X).
squareCoord(S, X) :- rank(S, X).


offsetSquare([X,Y], [OffsetX, OffsetY], [X2, Y2]) :-
    plus(X, OffsetX, X2),
    plus(Y, OffsetY, Y2).


squareSeries(S, Offsets, [S2 | Rest]) :-
    offsetSquare(S, Offsets, S2), square(S2),
    squareSeries(S2, Offsets, Rest).

squareSeries(S, Offsets, []) :-
    offsetSquare(S, Offsets, S2),
    outsideBoard(S2), !.


outsideBoard(X) :- integer(X), X < 0.
outsideBoard(X) :- integer(X), X > 7.
outsideBoard(S) :- squareCoord(S, X), outsideBoard(X).


diagonal(S, D) :-
    choose(2, [1,-1], Offset),
    squareSeries(S, Offset, D).


diagonal(S1, S2, D) :-
    square(S1),
    square(S2),
    diagonal(S1, D),
    member(S2, D).

:- begin_tests(chess).

test(diagonal) :-
    diagonal([3,4], D),
    length(D,4).

test(diagonal3) :-
    diagonal([3,4], [0,1], _).

:- end_tests(chess).
