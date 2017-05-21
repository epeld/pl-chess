:- module(convert, []).

%
% This 'script' converts svg images to bitmaps of a certain size
%

convert_images(Size) :-
  image_names(Size, SvgFile, PngFile, XpmFile),
  export_image(Size, SvgFile, PngFile, XpmFile),

  % failure driven loop
  fail.


export_image(Size, SvgFile, PngFile, XpmFile) :-
  format("Exporting ~s -> ~s -> ~s\n", [SvgFile, PngFile, XpmFile]),
  
  format(string(Command), "inkscape -z -e ~s ~s -w ~d -h ~d",
         [PngFile, SvgFile, Size, Size]),
  format(string(Command2), "convert ~s ~s", [PngFile, XpmFile]),
  
  format("> ~s", [Command]),
  shell(Command, Status),
  format(" --> ~d\n", [Status]),
  
  format("> ~s", [Command2]),
  shell(Command2, Status2),
  format(" --> ~d\n", [Status2]),

  % Delete the png
  delete_file(PngFile).


image_names(Size, SvgFile, PngFile, XpmFile) :-
  vector_name(Pt, C, SvgFile),
  image_name(Pt, C, Size, 'png', PngFile),
  image_name(Pt, C, Size, 'xpm', XpmFile).


image_name(Pt, Color, Size, Extension, Name) :-
  piece_name(P, Pt),
  color_name(C, Color),
  format(atom(Name), "~a/~a~a_~d.~s", ['bitmaps', C, P, Size, Extension]).


vector_name(Pt, Color, Name) :-
  piece_name(P, Pt),
  color_name(C, Color),
  format(atom(Name), "~a/~a~a.svg", ['vectors', C, P]).

color_name('White', white).
color_name('Black', black).

piece_name('Bishop', bishop).
piece_name('Knight', knight).
piece_name('Queen', queen).
piece_name('King', king).
piece_name('Pawn', pawn).
piece_name('Rook', rook).
