:- module(fen_service,
          [
            parse_string/2,
            initial_fen_string/1
          ]).
:- use_module(logic/fen,
              [
                string/2,
                initial_fen_string/1
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
