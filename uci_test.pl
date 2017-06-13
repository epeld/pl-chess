

:- set_prolog_flag(double_quotes, codes).


:- begin_tests(parse_options).

test(spin) :-
  phrase(option(Name, Spin), "option name Contempt Factor type spin default 0 min -50 max 50"),
  Spin = spin(0, -50, 50).


:- end_tests(parse_options).


example_options("option name Use Debug Log type check default false
option name Use Search Log type check default false
option name Search Log Filename type string default SearchLog.txt
option name Book File type string default book.bin
option name Best Book Move type check default false
option name Contempt Factor type spin default 0 min -50 max 50
option name Mobility (Middle Game) type spin default 100 min 0 max 200
option name Mobility (Endgame) type spin default 100 min 0 max 200
option name Passed Pawns (Middle Game) type spin default 100 min 0 max 200
option name Passed Pawns (Endgame) type spin default 100 min 0 max 200
option name Space type spin default 100 min 0 max 200
option name Aggressiveness type spin default 100 min 0 max 200
option name Cowardice type spin default 100 min 0 max 200
option name Min Split Depth type spin default 4 min 4 max 12
option name Max Threads per Split Point type spin default 5 min 4 max 8
option name Threads type spin default 2 min 1 max 64
option name Use Sleeping Threads type check default true
option name Hash type spin default 32 min 1 max 8192
option name Clear Hash type button
option name Ponder type check default true
option name OwnBook type check default false
option name MultiPV type spin default 1 min 1 max 500
option name Skill Level type spin default 20 min 0 max 20
option name Emergency Move Horizon type spin default 40 min 0 max 50
option name Emergency Base Time type spin default 200 min 0 max 30000
option name Emergency Move Time type spin default 70 min 0 max 5000
option name Minimum Thinking Time type spin default 20 min 0 max 5000
option name Slow Mover type spin default 100 min 10 max 1000
option name UCI_Chess960 type check default false
option name UCI_AnalyseMode type check default false").


example_id_string("id author Tord Romstad, Marco Costalba and Joona Kiiski").
example_init_string("Stockfish 09-06-13 64bit by Tord Romstad, Marco Costalba and Joona Kiiski").

example_bestmove_string("bestmove a2a3 ponder (none)").

example_info_string("info nodes 2 time 2").
example_info_string("info depth 3 seldepth 2 score cp 12 nodes 253 nps 253000 time 1 multipv 2 pv b1c3 g8f6 g1f3 b8c6").


uci_test(PgnMoves, S, _N) :-
  fen:second_position(P),
  fen:turn(P, black),
  uci:uci_moves(UciMoves, "g8f6 d2d3 a7a6 b1d2", []),
  uci:uci_pgn_moves(P, UciMoves, PgnMoves),
  string_helper(P, PgnMoves, S).
  % format("Move: ~s ~n", [S]).


string_helper(P, PgnMoves, S) :-
  pgnfile:pgn_move_string(P, PgnMoves, S), !.


uci_test2(PgnMove) :-
  fen:string(P, "rnbqkb1r/pp1ppppp/5n2/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"),
  PgnMove = pawn_move(nothing,move,square(3,2),nothing),
  pgn:full_move(P, PgnMove, pawn_move(square(1,1),move,square(3,2),nothing)).


profile_test :-
  profile(many_test).


many_test :-
  foreach(between(1, 100, N),
          uci_test(A, S, N)).


many_test2 :-
  foreach(between(1, 1000, N),
          uci_test2(A)).
