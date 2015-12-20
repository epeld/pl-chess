
:- module(movement, [
        knights_jump/2, 
        distance/3, 
        diagonal/2,
        diagonal/3,
        horizontal/2,
        vertical/2,
        piece_can_reach/4,
        piece_can_capture/4
    ]).

:- use_module(library(clpfd)).
:- use_module(square).

knights_jump(Square1, Square2) :-
    square([X1, Y1], Square1), 
    square([X2, Y2], Square2),

    abs(X2 - X1) + abs(Y2 - Y1) #= 3, 
    abs(Y2 - Y1) #>= 1,
    abs(X2 - X1) #>= 1.
    

distance(Square1, Square2, Range) :-
    square([X1, Y1], Square1), 
    square([X2, Y2], Square2),

    abs(X2 - X1) + abs(Y2 - Y1) #= Range.


diagonal(Square1, Square2) :-
    square([X1, Y1], Square1), 
    square([X2, Y2], Square2),

    abs(X2 - X1) #= abs(Y2 - Y1), 
    X2 #\= X1.


diagonal(Square1, Square2, SquareOnDiagonal) :-
    diagonal(Square1, Square2),

    square([X1, Y1], Square1), 
    square([X2, Y2], Square2), 
    square([X3, Y3], SquareOnDiagonal),

    min(X1, X2) #< X3, X3 #< max(X1, X2),
    min(Y1, Y2) #< Y3, Y3 #< max(Y1, Y2),

    diagonal(Square1, SquareOnDiagonal).


horizontal(Square1, Square2) :-
    square([_, Y1], Square1), 
    square([_, Y1], Square2),

    not_equal(Square1, Square2).

horizontal(Square1, Square2, Between) :-
    horizontal(Square1, Square2),
    horizontal(Square1, Between),
    horizontal_between(Square1, Square2, Between).

horizontal_between(Square1, Square2, Between) :- left_of(Square1, Between), right_of(Square2, Between).
horizontal_between(Square1, Square2, Between) :- left_of(Square2, Between), right_of(Square1, Between).
    

vertical(Square1, Square2) :-
    square([X2, _], Square1), 
    square([X2, _], Square2),

    not_equal(Square1, Square2).

vertical(Square1, Square2, Between) :-
    vertical(Square1, Square2),
    vertical(Square1, Between),
    vertical_between(Square1, Square2, Between).

vertical_between(Square1, Square2, Between) :- below(Square1, Between), above(Square2, Between).
vertical_between(Square1, Square2, Between) :- below(Square2, Between), above(Square1, Between).


straight(Square1, Square2) :- vertical(Square1, Square2) ; horizontal(Square1, Square2).

straight(Square1, Square2, Between) :- vertical(Square1, Square2, Between) ; horizontal(Square1, Square2, Between).

can_reach(queen, Source, Target, Between) :- can_reach(rook, Source, Target, Between) ; can_reach(bishop, Source, Target, Between).

can_reach(king, Source, Target, []) :- distance(Source, Target, 2), diagonal(Source, Target).
can_reach(king, Source, Target, []) :- distance(Source, Target, 1), straight(Source, Target).

can_reach(knight, Source, Target, []) :- knights_jump(Source, Target).

can_reach(bishop, Source, Target, []) :- distance(Source, Target, 2), diagonal(Source, Target). 
can_reach(bishop, Source, Target, Between) :- setof(X, diagonal(Source, Target, X), Between).

can_reach(rook, Source, Target, []) :- distance(Source, Target, 1), straight(Source, Target).
can_reach(rook, Source, Target, Between) :- setof(X, straight(Source, Target, X), Between).


can_capture(Officer, Source, Target, Between) :- officer(Officer), can_reach(Officer, Source, Target, Between).


% Determine if a piece can capture a given square
% First argument is a piece
% Second argument is the source square
% Third argument is target square
% Fourth argument will be a list of all squares in between source and target
piece_can_capture([_, Officer], Source, Target, Between) :- 
    officer(Officer), 
    can_capture(Officer, Source, Target, Between).

piece_can_capture([white, pawn], Source, Target, []) :-
    distance(Source, Target, 2),
    diagonal(Source, Target),
    below(Source, Target).

piece_can_capture([black, pawn], Source, Target, []) :-
    distance(Source, Target, 2),
    diagonal(Source, Target),
    below(Target, Source).


% Determine if a piece can reach a given square
% First argument is a piece
% Second argument is the source square
% Third argument is target square
% Fourth argument will be a list of all squares in between source and target
piece_can_reach([_, Officer], Source, Target, Between) :-
    officer(Officer),
    can_reach(Officer, Source, Target, Between).

piece_can_reach([white, pawn], Source, Target, []) :-
    distance(Source, Target, 1),
    vertical(Source, Target),
    below(Source, Target).

piece_can_reach([black, pawn], Source, Target, []) :-
    distance(Source, Target, 1),
    vertical(Source, Target),
    below(Target, Source).


piece_can(captures, Pc, Source, Target, Between) :-
    piece_can_capture(Pc, Source, Target, Between).

piece_can(moves, Pc, Source, Target, Between) :-
    piece_can_reach(Pc, Source, Target, Between).


not_equal(Square1, Square2) :-
    square:square([X1, Y1], Square1),
    square:square([X2, Y2], Square2),
    (X1 #\= X2 ; Y1 #\= Y2).


below(Square1, Square2) :-
    square:square([_, Y1], Square1),
    square:square([_, Y2], Square2),
    Y1 #< Y2.

above(Square1, Square2) :-
    square:square([_, Y1], Square1),
    square:square([_, Y2], Square2),
    Y1 #> Y2.

left_of(Square1, Square2) :-
    square:square([X1, _], Square1),
    square:square([X2, _], Square2),
    X1 #< X2.

right_of(Square1, Square2) :-
    square:square([X1, _], Square1),
    square:square([X2, _], Square2),
    X1 #> X2.


officer(rook).
officer(bishop).
officer(queen).
officer(king).
officer(knight).
