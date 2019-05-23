:- module(fen_service,
          [
            parse_string/2
          ]).
:- use_module(logic/fen, [string/2]).


parse_string(FenString, Position) :-
  ground(Position),
  fen:string(FenString, Position).
