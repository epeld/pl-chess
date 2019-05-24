:- module(pgn_service,
          [
            parse_pgn_string/2,
            make_pgn_move/3,
            find_possible_destinations/3
          ]).
:- use_module(logic/pgn,
              [
                pgn_string/2,
                make_move/3,
                source_destination/3
              ]).


parse_pgn_string(PgnString, ParsedMove) :-
  ground(PgnString),
  pgn:pgn_string(ParsedMove, PgnString),
  !.

make_pgn_move(Position, Move, Position2) :-
  make_move(Move, Position, Position2),
  !.


find_possible_destinations(Position, SourceSquare, Destinations) :-
  findall(
    Destination,
    pgn_service:source_destination(Position, SourceSquare, Destination),
    Destinations
  ).
