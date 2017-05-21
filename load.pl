
:- use_module(fen).
:- use_module(pgn).
:- use_module(movement).
:- use_module(color).
:- use_module(position).
:- use_module(state).
:- use_module(zipper).
:- use_module(main).




load_gui :-
  use_module(square),
  use_module(myframe),
  use_module(bitmaps),
  use_module(convert),

  bitmaps:load_bitmaps(64),
  
  % compile_resources,
  
  myframe:hello(A).


compile_resources :-
  [convert],
  convert_images(64).
