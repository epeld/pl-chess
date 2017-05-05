
:- module(main, []).

evaluate(position, P, _, P).

evaluate(move, [Move | Moves], P, P2) :-
  pgn:full_move(P, Move, FullMove),
  pgn:pgn_string(FullMove, Str),
  format("~s\n", [Str]), % TODO plug in different notations here
  position:position_after(FullMove, P, P1),
  evaluate(move, Moves, P1, P2).

evaluate(move, [], P, P).

command(abort, abort) --> "abort".

command(position, P) -->
  position(P).

command(move, Moves) -->
  move(Moves).


position(P) -->
  "position ", fen:position(P).

move(Moves) -->
  "move ", many_moves(Moves).


many_moves([Move | Moves]) -->
  pgn:move(Move),

  ( " ", many_moves(Moves)
  ; { Moves = [] } ).


repl :-
  fen:initial_position(P),
  repl(P, []).

repl(Position, Transcript) :-
  fen:string(Position, Str),
  format("Position: ~s\n", [Str]),
  format("> "),
  read_line_to_codes(user_input, Line),

  command(Command, Arg, Line, []),

  % Allow the user to terminate if he wishes:
  ( Command = abort,
    !
  
  ; !,
    evaluate(Command, Arg, Position, Position2), 
    repl(Position2, [[Command, Arg] | Transcript]) ).

repl(Position, Transcript) :-
  !,
  format("Error! Something went wrong. \n"),
  repl(Position, Transcript).
