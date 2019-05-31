:- module(fen_service,
          [
            parse_string/2,
            parse_square/2,
            initial_fen_string/1,
            encode_position/2,
            encode_square/2,
            encode_piece/2
          ]).
:- use_module(logic/fen,
              [
                string/2,
                initial_fen_string/1,
                square_codes/2,
                piece_char/2
              ]).

%
% Deterministic.
% Parses a fen string and unifies the resulting position with Position
%
parse_string(FenString, Position) :-
  ground(FenString),
  fen:string(Position, FenString).


%
% Deterministic.
% Encodes a position as a Fen string
%
encode_position(Position, FenString) :-
  ground(Position),
  fen:string(Position, FenString).


parse_square(SquareString, Square) :-
  ground(SquareString),
  square_codes(Square, SquareString).


encode_square(Square, SquareString) :-
  ground(Square),
  square_codes(Square, SquareString).

encode_piece(Piece, [Char]) :-
  ground(Piece),
  piece_char(Piece, Char).
