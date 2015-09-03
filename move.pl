:- module(move, [
    move/2, 
    piece_move/1,
    castling_move/1,
    moved_piece/2,
    source_indicator/2,
    move_type/2,
    destination/2,
    promotion/2]).

:- use_module(library(clpfd)).
:- use_module(square).
:- use_module(listutils).


% a piece move will be represented by a list of 3-4 elements
move([pawn | Rest], [pawn | Rest]) :- length(Rest, 4).
move([Officer | Rest], [Officer | Rest]) :- officer(Officer), length(Rest, 3).


% Non-castling move:
piece_move([Piece | _]) :- piece(Piece).


castling_move([castling, Side]) :- castling_side(Side).


moved_piece(Move, Piece) :- piece_move(Move), nth0(0, Move, Piece).

source_indicator(Move, Source) :- piece_move(Move), nth0(1, Move, Source).

move_type(Move, MoveType) :- piece_move(Move), nth0(2, Move, MoveType).

destination(Move, Destination) :- piece_move(Move), nth0(3, Move, Destination).

promotion(Move, Promotion) :- piece_move(Move), nth0(4, Move, Promotion).


piece(pawn).
piece(Officer) :- officer(Officer).


officer(bishop).
officer(knight).
officer(rook).
officer(king).
officer(queen).


castling_side(queenside).
castling_side(kingside).


% A move is uniquely determined in a given position
% whenever there is only one possible source square
% that it can have
uniquely_determined(Move, Position) :-
    bagof(X, possible_source_square(Move, Position, X), [_]).


legal_source_square(Move, Pos, Square) :-
    full_move(Move, FullMove, Square),
    legal(FullMove, Pos).


% FullMove is an 'Upgrade' of Move with source_indicator set to Square
full_move(Move, FullMove, Square) :-
    piece_move(Move),
    possible_source_square(Move, Pos, Square),
    replace(1, Move, FullMove, Square).


% A square is a possible source square of a move
% if it is occupied by the right piece (of the right color)
% and the square matches the move's source indicator
% TODO: and the resulting positon would be legal
possible_source_square(Move, Pos, Square) :-
    compatible_source_square(Move, Square),
    moved_piece(Move, Piece),
    position_turn(Pos, Color),
    board_occupant(Pos, Square, [Piece, Color]),
    legal(Move, Pos).


compatible_source_square(Move, Square) :-
    source_indicator(Move, Indicator),
    square_indicator(Square, Indicator).


square_indicator(_, nothing).
square_indicator([File, _], File).
square_indicator([_, Rank], Rank).
square_indicator(X, X).


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
