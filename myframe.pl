:- module(myframe, []).

:- use_module(library(clpfd)).
:- use_module(library(pce)).

:- pce_begin_class(chess_board, picture).

variable(squares, vector, get).

resize(Frame) :->
  send_super(Frame, resize),

  get(Frame, width, W),
  get(Frame, height, H),

  % default_size([W, H]),

  TileW is W // 8,
  TileH is H // 8,

  get(Frame, squares, Chain),
  send(Chain, for_all, message(@arg1, reconfigure, TileW, TileH)).


initialise(Self) :->
  send_super(Self, initialise, 'Chessboard'),

  default_size([Width, Height]),

  send(Self, width, Width),
  send(Self, height, Height),

  new(V, vector),
  send(Self, slot, squares, V),
  
  forall(between(0, 63, Ix),
        (
          make_square(Ix, Square),
          send(Self, display, Square),
          send(V, append, Square)
        )).



square(Self, Index, Square) :<-
  get(Self, squares, V),
  Index0 is Index + 1,
  get(V, element(Index0), Square).


char(Self, Index, Char) :->
  get(Self, square(Index), Square),
  send(Square, char, Char).


:- pce_end_class.

make_square(Ix, Square) :-
  new(Square, square),
  send(Square, square_index, Ix).


default_size([S, S]) :-
  S is 64 * 8.

hello(A) :-
  new(A, chess_board),
  send(A, open).



display(Board, FEN) :-
  fen:position(P, FEN, []),
  fen:board(P, [board, Rows]),
  append(Rows, AllRows),
  maplist(maybe_piece_char, AllRows, AllChars),
  display_chars(Board, AllChars).


display_chars(Board, AllChars) :-
  forall(nth0(Ix, AllChars, Char),
         send(Board, char(Ix, Char))).


maybe_piece_char([PieceType, Color], Char) :-
  fen:piece_char([PieceType, Color], Char).

maybe_piece_char(nothing, none).
