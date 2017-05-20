

convert_images(Size) :-
  svg_images(Images),
  convert_images(Size, Images),
  fail.

convert_images(Size, Images) :-
  member(Image, Images),
  export_image(Image, Size).

export_image(Image, Size) :-
  exported_png_name(Size, Image, Exported),
  format("Exporting ~s as ~s\n", [Image, Exported]),
  format(string(Command), "inkscape -z -e ~s ~s", [Exported, Image]),
  format("> ~s", [Command]),
  shell(Command, Status),
  format(" --> ~d", [Status]).

svg_images(Files) :-
  expand_file_name("vectors/*", Files).


special_file('.').
special_file('..').


svg_file(File) :-
  file_name_extension(_Name, 'svg', File).


exported_png_name(Size, SvgFile, PngFile) :-
  file_name_extension(Name0, 'svg', SvgFile),
  file_base_name(Name0, Name),

  image_name(Name),

  format(string(Name2), "~s/~s_~d", ['bitmaps', Name, Size]),
  
  file_name_extension(Name2, 'xpm', PngFile).


image_name(Name) :-
  piece_name(P),
  color_name(C),
  format(atom(Name), "~s~s", [C, P]).

color_name('White').
color_name('Black').

piece_name('Bishop').
piece_name('Knight').
piece_name('Queen').
piece_name('King').
piece_name('Pawn').
piece_name('Rook').
