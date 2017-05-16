
:- set_prolog_flag(double_quotes, codes).

not_mate("rnbqkb1r/ppp2Bpp/3p1n2/4p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 4").


verify_everything_works :-
  not_mate(FEN),
  fen:string(P, FEN),
  !,
  \+ pgn:checkmate(P).


% TODO this repl session behaves strangely:
% > move e4 e5 e1
% test and fix!
