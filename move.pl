

move_type([_,_,Type,_,_], Type).
move_source_indicator([_,Source,_,_,_], Source).
move_destination([_,_,_,Destination,_], Destination).
move_meta([_,_,_,_,Meta], Meta).
move_piecetype([PieceType,_,_,_,_], PieceType).

% Abstraction.
move(Move, Move).

source_indicator([Type, Val], Type, Val) :- valid_indicator_type(Type).

valid_indicator_type(rank).
valid_indicator_type(file).
valid_indicator_type(square).
