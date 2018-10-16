:- module(main, [main/0, main_with_args/1]).

:- set_prolog_flag(double_quotes, codes).

:- use_module(fen, [string/2]).
:- use_module(pgn, [pgn_string/2]).

main :-
  current_prolog_flag(argv, Argv),
  main_with_args(Argv) *->
    true
  ; print_usage.


main_with_args([Fen, Pgn]) :-
  format("Fen: ~w~n", [Fen]),
  format("Pgn: ~w~n", [Pgn]),
  once(
    parse_fen(Fen, Position)
  ) *-> (
        once(
          parse_pgn(Pgn, Move)
        ) *->
        main_with_position(Position, Move)
      ; format(user_error, "Unable to parse pgn \"~w\"~n", [Pgn])
      )
  ; format(user_error, "Unable to parse fen \"~w\"~n", [Fen]).


print_usage :-
  format(user_error, "Usage: chess <fen> <pgn>~nWhere \"chess\" is the name of the executable~n", []).


main_with_position(_Position, Move) :-
  format("OK~nMove is ~w~n", [Move]).

parse_fen(Fen, Position) :-
  atom_codes(Fen, CFen),
  fen:string(Position, CFen).

parse_pgn(Pgn, Move) :-
  atom_codes(Pgn, CPgn),
  pgn:pgn_string(Move, CPgn).
