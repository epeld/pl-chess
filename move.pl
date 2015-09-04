:- module(move, [
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

source_indicator(Move, Source) :- piece_move(Move), nth0(1, Move, Source).

move_type(Move, MoveType) :- piece_move(Move), nth0(2, Move, MoveType).

destination(Move, Destination) :- piece_move(Move), nth0(3, Move, Destination).

promotion(Move, Promotion) :- piece_move(Move), nth0(4, Move, Promotion).


% FullMove is Move but with source indicator upgraded to be Square
full_source(Move, Square, FullMove) :-
    square:square(Square), piece_move(Move),
    replacement_at(1, Move, ResultingMove, Square).


piece(pawn).
piece(Officer) :- officer(Officer).


officer(bishop).
officer(knight).
officer(rook).
officer(king).
officer(queen).


castling_side(queenside).
castling_side(kingside).
