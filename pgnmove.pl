%
%  This module represents a typical PGN move. There are two types:
%  - castling moves
%  - piece moves
%
%  Piece moves consist of [PieceType, SourceIndicator, MoveType, Destination, Promotion?]
%  See the corresponding predicates below for more info.
%
%  Castling moves are represented by the atoms queenside/kingside.
%
:- module(pgnmove, [
        move/2, 
        piece_move/1,
        castling_move/1,
        moved_piece/2,
        source_indicator/2,
        move_type/2,
        destination/2,
        promotion/2
    ]).


:- use_module(movement).
:- use_module(square).
:- use_module(listutils).


% a piece move will be represented by a list of 3-4 elements
move([pawn | Rest], [pawn | Rest]) :- length(Rest, 4).
move([Officer | Rest], [Officer | Rest]) :- officer(Officer), length(Rest, 3).


% Non-castling move:
piece_move([Piece | _]) :- piece(Piece).


castling_move([castling, Side]) :- castling_side(Side).


moved_piece(Move, Piece) :- piece_move(Move), nth0(0, Move, Piece).


%  A source indicator indicates what could be the source square of a move.
%  It is understood that the source indicator only need provide enough information
%  to uniquely identify a move, given a position. It may thus only consist of
%  a file, a rank, a square or even nothing!
source_indicator(Move, Source) :- 
    source_indicator(Source),
    piece_move(Move), 
    nth0(1, Move, Source).

source_indicator(File) :- square:file(File).

source_indicator(Rank) :- square:rank(Rank).

source_indicator(nothing).

source_indicator(Square) :- square:square(Square).


move_type(Move, MoveType) :- 
    move_type(MoveType),
    piece_move(Move), 
    nth0(2, Move, MoveType).

move_type(captures).
move_type(moves).


destination(Move, Destination) :- 
    square:square(Destination),
    piece_move(Move), 
    nth0(3, Move, Destination).


promotion(Move, Promotion) :- 
    promotee(Promotion),
    piece_move(Move), 
    nth0(4, Move, Promotion).


compatible_indicator(_, nothing).
compatible_indicator(Square, File) :- square:square_file(Square, File).
compatible_indicator([_, Rank], Rank) :- square:square_rank(Square, Rank).
compatible_indicator(X, X) :- square:square(X).


compatible_source_square(Move, Square) :-
    source_indicator(Move, Indicator),
    compatible_indicator(Square, Indicator).


% FullMove is Move but with compatible source square Square
square_source(Move, Square, FullMove) :-
    square:square(Square), piece_move(Move),
    compatible_source_square(Move, Square),
    replacement_at(1, Move, ResultingMove, Square).


piece(pawn).
piece(Officer) :- officer(Officer).


officer(bishop).
officer(knight).
officer(rook).
officer(king).
officer(queen).


promotee(nothing).
promotee(knight).
promotee(bishop).
promotee(queen).
promotee(rook).


castling_side(queenside).
castling_side(kingside).
