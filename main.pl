
main :-
  current_prolog_flag(argv, Argv),
  main_with_args(Argv) *->
    true
  ; print_usage.


main_with_args([Fen, Pgn]) :-
  format("Fen: ~w~n", [Fen]),
  format("Pgn: ~w~n", [Pgn]).


print_usage :-
  format(user_error, "Usage: chess <fen> <pgn>~nWhere \"chess\" is the name of the executable~n", []).
