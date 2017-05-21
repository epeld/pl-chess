%
% This 'script' converts svg images to bitmaps of a certain size
%

convert_images(Size) :-
  svg_images(Images),
  convert_images(Size, Images),
  fail.

convert_images(Size, Images) :-
  member(Image, Images),
  export_image(Image, Size).

export_image(Image, Size) :-
  exported_png_name(Size, Image, Exported, Exported2),
  format("Exporting ~s as ~s\n", [Image, Exported]),
  
  format(string(Command), "inkscape -z -e ~s ~s", [Exported, Image]),
  format(string(Command2), "convert ~s ~s", [Exported, Exported2]),
  
  format("> ~s", [Command]),
  shell(Command, Status),
  format(" --> ~d\n", [Status]),
  
  format("> ~s", [Command2]),
  shell(Command2, Status2),
  format(" --> ~d\n", [Status2]),

  % Delete the png
  delete_file(Exported).

svg_images(Files) :-
  expand_file_name("vectors/*", Files).


special_file('.').
special_file('..').


svg_file(File) :-
  file_name_extension(_Name, 'svg', File).


exported_png_name(Size, SvgFile, PngFile, XpmFile) :-
  file_name_extension(Name0, 'svg', SvgFile),
  file_base_name(Name0, Name),

  image_name(Name),

  format(string(Name2), "~s/~s_~d", ['bitmaps', Name, Size]),
  
  file_name_extension(Name2, 'png', PngFile),
  file_name_extension(Name2, 'xpm', XpmFile).


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
