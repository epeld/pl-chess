
not_mate("rnbqkb1r/ppp2Bpp/3p1n2/4p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 4").


verify_everything_works :-
  not_mate(FEN),
  fen:string(P, FEN),
  !,
  \+ pgn:checkmate(P).
