
:- module(movement, [
        knights_jump/2, 
        distance/3, 
        diagonal/2,
        diagonal/3,
        horizontal/2,
        vertical/2
    ]).

:- use_module(library(clpfd)).
:- use_module(square).

knights_jump(Square1, Square2) :-
    square([X1, Y1], Square1), square([X2, Y2], Square2),
    abs(X2 - X1) + abs(Y2 - Y1) #= 3, 
    abs(Y2 - Y1) #>= 1,
    abs(X2 - X1) #>= 1.
    

distance(Square1, Square2, Range) :-
    square([X1, Y1], Square1), square([X2, Y2], Square2),
    abs(X2 - X1) + abs(Y2 - Y1) #=< Range.


diagonal(Square1, Square2) :-
    square([X1, Y1], Square1), square([X2, Y2], Square2),
    abs(X2 - X1) #= abs(Y2 - Y1), X2 #\= X1.


diagonal(Square1, Square2, SquareOnDiagonal) :-
    diagonal(Square1, Square2),
    square([X1, Y1], Square1), square([X2, Y2], Square2), square([X3, Y3], SquareOnDiagonal),
    min(X1, X2) #< X3, X3 #< max(X1, X2),
    min(Y1, Y2) #< Y3, Y3 #< max(Y1, Y2),
    diagonal(Square1, SquareOnDiagonal).


horizontal(Square1, Square2) :-
    square([_, Y1], Square1), square([_, Y1], Square2).
    

vertical(Square1, Square2) :-
    square([X2, _], Square1), square([X2, _], Square2).
