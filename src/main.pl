:- module(main, [main/0, main_with_args/1]).

:- set_prolog_flag(double_quotes, codes).

:- use_module(services/fen_service, [parse_string/2]).
:- use_module(services/logic/pgn, [pgn_string/2, make_move/3]).

main :-
  current_prolog_flag(argv, Argv),
  main_with_args(Argv) *->
    true
  ; print_usage.


main_with_args([Fen, Pgn]) :-
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


main_with_position(Position, Move) :-
  once(pgn:make_move(Move, Position, NextPosition)) *->
    (
      encode_fen(NextPosition, NextFEN),
      format("~s~n", [NextFEN])
    )
  ; format(user_error, "Error~n", []).

parse_fen(Fen, Position) :-
  as_codes(Fen, CFen),
  fen_service:parse_string(CFen, Position).

encode_fen(Position, Fen) :-
  fen:string(Position, Fen).

parse_pgn(Pgn, Move) :-
  as_codes(Pgn, CPgn),
  pgn:pgn_string(Move, CPgn).

as_codes(Any, Codes) :-
  format(codes(Codes), "~s", [Any]).
