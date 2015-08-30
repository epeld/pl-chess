

move([pawn | Rest], [pawn | Rest]) :- length(Rest, 4).
move([Officer | Rest], [Officer | Rest]) :- officer(Officer), length(Rest, 3).


officer(bishop).
officer(knight).
officer(rook).
officer(king).
officer(queen).
