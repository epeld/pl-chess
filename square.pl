:- module(square, []).


:- use_module(library(clpfd)).
:- use_module(library(pce)).

:- pce_begin_class(square, device).

variable(box, box, get, "The boxy box").
variable(bitmap, bitmap, get, "The bitmappy bitmap").
variable(text, text, get, "The texty text").
variable(index, int, get, "The indexy index").

initialise(Self) :->
  new(B, box(32, 32)),
  new(T, text('square')),
  new(Bmp, bitmap),

  send(Self, slot, box, B),
  send(Self, slot, text, T),
  send(Self, slot, bitmap, Bmp),
  send(Self, slot, index, 0), % initial value

  send_super(Self, initialise),

  send(B, pen, 0),

  send(Self, display, B),
  send(Self, display, T),
  send(Self, display, Bmp),

  send(Bmp, displayed(@off)),
  send(T, displayed(@off)).


square_index(Self, Index) :->
  send(Self, slot, index, Index),
  get(Self, box, Box),

  % String
  square_index(Row, Col, Index),
  square_coords(Sq, Row, Col),

  fen:square_codes(Sq, Codes),
  string_codes(String, Codes),

  get(Self, text, T),
  send(T, string, String),

  % Colour
  square_colour(Index, Colour),
  send(Box, fill_pattern, colour(Colour)).


char(Self, Char) :->
  get(Self, bitmap, Bmp),
  (Char = none *->
    send(Bmp, displayed(@off))
  ; bitmaps:piece_image(Char, 64, Image),
    send(Bmp, image, Image),
    send(Bmp, displayed(@on)),
    get(Self, box, B),
    get(B, center, BC),
    send(Bmp, center, BC)).


reconfigure(Self, Width, Height) :->
  get(Self, index, Index),

  % Resize box
  get(Self, box, Box),
  send(Box, width, Width),
  send(Box, height, Height),
  
  % Position
  square_index(Row, Col, Index),
  X #= Col * Width,
  Y #= Row * Height,
  send(Self, move, point(X, Y)).


text_displayed(Self, Displayed) :->
  get(Self, text, T),
  send(T, displayed, Displayed).

:- pce_end_class.


square_coords([square, Col, FRow], Row, Col) :-
  fen:fen_y_coord(FRow, Row).

square_index(Row, Col, Index) :-
  Index #= Row * 8 + Col,
  Index #>= 0,
  Index #< 64,
  Row #< 8, Row #>= 0,
  Col #< 8, Col #>= 0.


square_name(Name, Index) :-
  square_index(Sq, Index),
  fen:square_codes(Sq, Name).


square_colour(Index, Color) :-
  A is (Index // 8) mod 2,
  B is (Index mod 8) mod 2,
  1 is (A + B) mod 2 *->
    Color = gray
  ; Color = white.


hello(P, D) :-
  new(P, picture),
  new(D, square),

  send(P, open),
  send(P, display, D).

