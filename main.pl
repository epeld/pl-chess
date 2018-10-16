
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
      ; format(user_error, "Unable to parse pgn ~w~n", [Pgn])
      )
  ; format(user_error, "Unable to parse fen ~w~n", [Fen]).


print_usage :-
  format(user_error, "Usage: chess <fen> <pgn>~nWhere \"chess\" is the name of the executable~n", []).


main_with_position(_Position, _Move) :-
  format("OK~n").

parse_fen(_Fen, dummy_fen).
parse_pgn(_Pgn, dummy_pgn).
