:- module(pgn_service,
          [
            parse_pgn_string/2,
            make_move/3
          ]).
:- use_module(logic/pgn,
              [
                pgn_string/2,
                make_move/3
              ]).


parse_pgn_string(PgnString, ParsedMove) :-
  ground(PgnString),
  pgn:pgn_string(ParsedMove, PgnString),
  !.

% make_move(_Arg1, _Arg2, _Arg3) :- true.
