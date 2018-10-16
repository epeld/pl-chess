
main :-
  current_prolog_flag(argv, Argv),
  format("Args are ~w~n", [Argv]).
