
:- module(position, []).

%
%  This module handles updating a position, i.e after a move has been made
%

list_replace(Ix, Item, [X | List], [ X | Result]) :-
  nonvar(Ix), 
  Ix > 0,
  succ(Ix0, Ix),
  list_replace(Ix0, Item, List, Result).

list_replace(0, Item, [_ | List], [Item | List]).


% Note: This function is inspired by fen:piece_at predicate inside fen-module
board_replace(square(X, Y), NewPiece, [board, Rows], [board, NewRows]) :-
  movement:square(X, Y),

  fen:fen_y_coord(Y, Y0),
  
  nth0(Y0, Rows, Row),
  list_replace(X, NewPiece, Row, NewRow),
  list_replace(Y0, NewRow, Rows, NewRows).


position_after(castles(Side),
               Position,
               Position2) :-

  pgn:castling_possible(Side, Position),
  
  Position = [position, Board, Turn, Rights, _, HalfMoveNr, FullMoveNr],
  Position2 = [position, Board2, Turn2, Rights2, nothing, HalfMoveNr2, FullMoveNr2],
  
  succ(HalfMoveNr, HalfMoveNr2),
  succ(FullMoveNr, FullMoveNr2),

  color:opposite(Turn, Turn2),

  exclude(castling_right(_, Turn), Rights, Rights2),

  color:initial_king_square(Turn, KingSquare),
  color:initial_rook_square(Turn, Side, RookSquare),
  
  color:castled_rook_square(Turn, Side, RookSquare2),
  color:castled_king_square(Turn, Side, KingSquare2),

  board_replace(RookSquare, nothing, Board, Board_1),
  board_replace(RookSquare2, piece(rook, Turn), Board_1, Board_2),
  board_replace(KingSquare, nothing, Board_2, Board_3),
  board_replace(KingSquare2, piece(king, Turn), Board_3, Board2).


% (Non-passant) capture
position_after( [move, pawn, SourceSquare, capture, Destination, Promotion]
                , Position
                , [position, Board2, Turn2, Rights, nothing, 0, FullMoveNr2] ) :-

  Position = [position, Board, Turn, Rights, _, _, FullMoveNr],
  pgn:possible_move(capture, pawn, SourceSquare, Destination, Position),
  
  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),

  movement:pawn_capture_square(Turn, SourceSquare, Destination),

  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, piece(pawn, Turn), Board_1, Board_2),


  promote(Destination, Promotion, Board_2, Board2),
  % sanity check
  fen:piece_at(Board, Destination, piece(pawn, Turn2)),
  fen:piece_at(SourceSquare, Board, piece(pawn, Turn)).


position_after( [move, pawn, SourceSquare, move, Destination, Promotion]
                , Position
                , [position, Board2, Turn2, Rights, Passant2, 0, FullMoveNr2] ) :-

  Position = [position, Board, Turn, Rights, _, _, FullMoveNr],
  pgn:possible_move(move, pawn, SourceSquare, Destination, Position),
  
  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),


  movement:pawn_move_square(Turn, SourceSquare, Destination),
  movement:passant_square(Turn, SourceSquare, Destination, Passant2),

  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, piece(pawn, Turn), Board_1, Board_2),

  promote(Destination, Promotion, Board_2, Board2),

  % sanity check
  fen:piece_at(Board, Destination, nothing),
  fen:piece_at(Board, SourceSquare, piece(pawn, Turn)),
  ( fen:piece_at(Board, Passant2, nothing)
  ; Passant2 = nothing ).

% Passant Capture
position_after( [move, pawn, SourceSquare, capture, Destination, nothing]
                , Position
                , [position, Board2, Turn2, Rights, nothing, 0, FullMoveNr2] ) :-

  Position = [position, Board, Turn, Rights, Destination, _, FullMoveNr],
  
  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),

  % Figure out where the enemy pawn is
  movement:pawn_move_square(Turn, BehindPassant, Destination),

  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, piece(pawn, Turn), Board_1, Board_2),
  board_replace(BehindPassant, nothing, Board_2, Board2),

  % Sanity checks:
  fen:piece_at(Board, Destination, nothing),
  fen:piece_at(Board, BehindPassant, piece(pawn, Turn2)).


position_after( [move, Officer, SourceSquare, MoveType, Destination]
                , Position
                , [position, Board2, Turn2, Rights2, nothing, HalfMoveNr2, FullMoveNr2] ) :-
  Position = [position, Board, Turn, Rights, _, HalfMoveNr, FullMoveNr],

  movement:officer(Officer),
  pgn:possible_move(MoveType, Officer, SourceSquare, Destination, Position),

  % HalfMove = move since the last pawn move or capture
  ( MoveType = capture, HalfMoveNr2 = 0
  ; MoveType = move, succ(HalfMoveNr, HalfMoveNr2) ),

  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),



  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, piece(Officer, Turn), Board_1, Board2),

  rights_after(SourceSquare, Destination, Rights, Rights2),
  
  % Sanity check:
  fen:piece_at(Board, SourceSquare, piece(Officer, Turn)).


positions_after_pgn(InitialPosition, Moves, Positions) :-
  ground(Moves), ground(InitialPosition),
  
  length(Moves, N), length(Positions, N), length(Positions_, N), length(RealMoves, N),
  
  maplist(pgn:pgn_string, RealMoves, Moves),
  
  positions_after_all(InitialPosition, RealMoves, Positions_),
  maplist(fen:string, Positions_, Positions).


positions_after_all(_, [], _).


positions_after_all(InitialPosition, [Move | Moves], [Position1 | Positions]) :-
  pgn:unique_full_move(InitialPosition, Move, FullMove),
  position_after(FullMove, InitialPosition, Position1),
  positions_after_all(Position1, Moves, Positions).

  


%
% Promotion helper
%
promote(square(X, Y), nothing, Board, Board) :-
  color:last_pawn_rank(Color, LastRank),
  LastRank =\= Y,
  fen:piece_at(Board, square(X, Y), piece(pawn, Color)).


promote(square(X, Y), Promotion, Board, Board2) :-
  movement:officer(Promotion),
  color:last_pawn_rank(Color, Y),
  fen:piece_at(Board, square(X, Y), piece(pawn, Color)),
  board_replace(square(X, Y), piece(Promotion, Color), Board, Board2).


%
%  Misc Helpers
%
next_full_move_nr(black, Nr, Nr2) :- succ(Nr, Nr2).
next_full_move_nr(white, Nr, Nr).

rights_after(Source, Destination, Rights, Rights2) :-
  rights_after(Source, Rights, Rights1),
  rights_after(Destination, Rights1, Rights2).


% e1
rights_after(square(4, 0), Rights, Rights2) :-
  delete(Rights, [_, white], Rights2).

% a1
rights_after(square(0, 0), Rights, Rights2) :-
  delete(Rights, [queenside, white], Rights2).

% h1
rights_after(square(7, 0), Rights, Rights2) :-
  delete(Rights, [kingside, white], Rights2).


% h8
rights_after(square(7, 7), Rights, Rights2) :-
  delete(Rights, [kingside, black], Rights2).

% a8
rights_after(square(0, 7), Rights, Rights2) :-
  delete(Rights, [queenside, black], Rights2).


% e8
rights_after(square(4, 7), Rights, Rights2) :-
  delete(Rights, [_, black], Rights2).

rights_after(square(X, Y), Rights, Rights) :-
  between(1, 6, X), between(0, 7, Y).

rights_after(square(0, Y), Rights, Rights) :-
  between(1, 6, Y).

rights_after(square(7, Y), Rights, Rights) :-
  between(1, 6, Y).

%  maplist(fen:square_codes, SpecialSquares, ["a1", "h1", "e1", "a8", "h8", "e8"]),
%  \+ member(Square, SpecialSquares).
  



