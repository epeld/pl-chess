:- module(bitmaps, []).

:- use_module(library(pce)).

:- pce_begin_class(bitmaps, object).

:- pce_global(@bitmaps, new(hash_table)).

:- pce_end_class.


load_bitmaps(Size) :-
  new(H, hash_table),
  send(@bitmaps, append(Size, H)),

  % Load new
  forall(convert:image_name(PieceType, Color, Size, 'xpm', Name),
         (
           new(Bitmap, bitmap(Name)),
           fen:piece_char([PieceType, Color], Char),
           send(H, append(Char, Bitmap))
         )),

  % Confirm
  format("Bitmaps ~dx~d loaded.\n", [Size, Size]).


piece_image(Char, Image) :-
  piece_bitmap(Char, Size, Bitmap),
  get(Bitmap, image, Image).

piece_bitmap(PieceType, Color, Size, Bitmap) :-
  fen:piece_char([PieceType, Color], Char),
  
  !,
  piece_bitmap(Char, Size, Bitmap).

piece_bitmap(Char, Size, Bitmap) :-
  get(@bitmaps, member(Size), H),
  get(H, member(Char), Bitmap).

piece(PT, C) :-
  convert:color_name(_, C),
  convert:piece_name(_, PT).
