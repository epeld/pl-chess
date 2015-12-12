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
        pawn_move/1,
        officer_move/1,
        castling_move/1,
        moved_officer_type/2,
        moved_piece_type/2,
        source_indicator/2,
        move_type/2,
        destination/2,
        promotion/2,
        source_square/3
    ]).


:- use_module(movement).
:- use_module(square).
:- use_module(listutils).


pawn_move(Move) :-
    moved_piece_type(Move, pawn),
    length(Move, 5). % Piece, Source, MoveType, Destination, promotee


officer_move(Move) :-
    moved_officer_type(Move, _), 
    length(Move, 4). % Piece, Source, MoveType, Destination


castling_move(Side) :- castling_side(Side).


% Non-castling move:
piece_move([Piece | _]) :- piece(Piece).


moved_piece_type(Move, Piece) :- piece_move(Move), nth0(0, Move, Piece).


moved_officer_type(Move, Officer) :- moved_piece_type(Move, Officer), officer(Officer).


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
    pawn_move(Move), 
    promotee(Promotion),
    nth0(4, Move, Promotion).


% FullMove is Move but with a full square as source indicator
source_square(Move, Square, FullMove) :-
    compatible_source_square(Move, Square),
    source_square(FullMove, Square).

source_square(Move, Square) :- 
    square:square(Square), piece_move(Move),
    compatible_source_square(Move, Square).


% Is a square compatible with a given source indicator?
compatible_indicator(_, nothing).
compatible_indicator([File, _], File) :- square:file(File).
compatible_indicator([_, Rank], Rank) :- square:rank(Rank).
compatible_indicator(X, X) :- square:square(X).


% A compatible source square is a square that *could* be the source
% of a square (i.e it is compatible with its source indicator)
compatible_source_square(Move, Square) :-
    source_indicator(Move, Indicator),
    compatible_indicator(Square, Indicator).


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
