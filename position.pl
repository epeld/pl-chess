
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
board_replace([square, X, Y], NewPiece, [board, Rows], [board, NewRows]) :-
  movement:square(X, Y),

  fen:fen_y_coord(Y, Y0),
  
  nth0(Y0, Rows, Row),
  list_replace(X, NewPiece, Row, NewRow),
  list_replace(Y0, NewRow, Rows, NewRows).


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
  board_replace(Destination, [pawn, Turn], Board_1, Board_2),


  promote(Destination, Promotion, Board_2, Board2),
  % sanity check
  fen:piece_at(Board, Destination, [pawn, Turn2]),
  fen:piece_at(SourceSquare, Board, [pawn, Turn]).


position_after( [move, pawn, SourceSquare, move, Destination, Promotion]
                , Position
                , [position, Board2, Turn2, Rights, nothing, 0, FullMoveNr2] ) :-

  Position = [position, Board, Turn, Rights, _, _, FullMoveNr],
  pgn:possible_move(move, pawn, SourceSquare, Destination, Position),
  
  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),


  movement:pawn_move_square(Turn, SourceSquare, Destination),
  movement:passant_square(Turn, SourceSquare, Destination, Passant2),

  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, [pawn, Turn], Board_1, Board_2),

  promote(Destination, Promotion, Board_2, Board2),
  
  % sanity check
  fen:piece_at(Board, Destination, nothing),
  fen:piece_at(Board, SourceSquare, [pawn, Turn]),

  ( fen:piece_at(Board, Passant2, nothing)
  ; Passant2 = nothing ).


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
  board_replace(Destination, [pawn, Turn], Board_1, Board_2),
  board_replace(BehindPassant, nothing, Board_2, Board2),

  % Sanity checks:
  fen:piece_at(Board, Destination, nothing),
  fen:piece_at(Board, BehindPassant, [pawn, Turn2]).


position_after( [move, Movement:Officer, SourceSquare, MoveType, Destination]
                , Position
                , [position, Board2, Turn2, Rights2, nothing, HalfMoveNr2, FullMoveNr2] ) :-
  Position = [position, Board, Turn, Rights, _, HalfMoveNr, FullMoveNr],
  
  movement:officer(Movement:Officer),
  Rights = Rights2, % TODO!

  % HalfMove = move since the last pawn move or capture
  ( MoveType = capture, HalfMoveNr2 = 0
  ; MoveType = move, succ(HalfMoveNr, HalfMoveNr2) ),

  next_full_move_nr(Turn, FullMoveNr, FullMoveNr2),
  color:opposite(Turn, Turn2),

  pgn:possible_move(MoveType, Movement:Officer, SourceSquare, Destination, Position),

  % Remove piece from source square
  board_replace(SourceSquare, nothing, Board, Board_1),
  board_replace(Destination, [pawn, Turn], Board_1, Board2),

  % Sanity check:
  fen:piece_at(SourceSquare, Board, [Movement:Officer, Turn]).



%
% Promotion helper
%
promote([square, X, Y], nothing, Board, Board) :-
  color:last_pawn_rank(Color, LastRank),
  LastRank =\= Y,
  fen:piece_at(Board, [square, X, Y], [pawn, Color]).


promote([square, X, Y], Promotion, Board, Board2) :-
  movement:officer(Promotion),
  color:last_pawn_rank(Color, Y),
  fen:piece_at(Board, [square, X, Y], [pawn, Color]),
  board_replace([square, X, Y], [Promotion, Color], Board, Board2).


%
%  Misc Helpers
%
next_full_move_nr(black, Nr, Nr2) :- succ(Nr, Nr2).

rights_after(Source, Destination, Rights, Rights2) :-
  Rights = Rights2. % TODO
