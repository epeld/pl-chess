:- module(move).

:- use_module(library(clpfd)).
:- use_module(square).


% a piece move will be represented by a list of 3-4 elements
move([pawn | Rest], [pawn | Rest]) :- length(Rest, 4).
move([Officer | Rest], [Officer | Rest]) :- officer(Officer), length(Rest, 3).


% Non-castling move:
piece_move([Piece | _]) :- piece_move(Piece).


castling_move([castling, Side]) :- castling_side(Side).


moved_piece(Move, Piece) :- piece_move(Move), nth0(0, Move, Piece).

source_indicator(Move, Source) :- piece_move(Move), nth0(1, Move, Source).

move_type(Move, MoveType) :- piece_move(Move), nth0(1, Move, MoveType).

destination(Move, Destination) :- piece_move(Move), nth0(1, Move, MoveType).

promotion(Move, Promotion) :- piece_move(Move), nth0(1, Move, MoveType).


piece(pawn).
piece(Officer) :- officer(Officer).


officer(bishop).
officer(knight).
officer(rook).
officer(king).
officer(queen).


castling_side(queenside).
castling_side(kingside).


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
