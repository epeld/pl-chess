:- module(myframe, []).

:- use_module(library(clpfd)).
:- use_module(library(pce)).

:- pce_begin_class(chess_board, picture).

variable(squares, chain, get).

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
  send_super(Self, initialise, 'Hello World'),

  default_size([Width, Height]),

  send(Self, width, Width),
  send(Self, height, Height),
  findall(Square,
        (
          between(0, 63, Ix),
          make_square(Ix, Square)
        ),
        Squares),

  forall(member(Square, Squares),
         send(Self, display, Square)),

  send(Self, slot, squares, Squares).


:- pce_end_class.

make_square(Ix, Square) :-
  new(Square, square),
  send(Square, assign_index, Ix).


default_size([S, S]) :-
  S is 64 * 8.

hello(A) :-
  new(A, chess_board),
  send(A, open).


