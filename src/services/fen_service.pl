:- module(fen_service,
          [
            parse_string/2
          ]).
:- use_module(fen, [string/2]).


parse_string(FenString, Position) :-
  bound(FenString),
  fen:string(FenString, Position).
