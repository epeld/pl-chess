
:- module(main, []).

%
% General command line parsing
%
command(Command, Args) -->
  {
    command_name(Command, Codes)
  },
  Codes,
  ( " ", command_args(Args)
  ; { Args = [] } ).


command_arg(Arg, Before, After) :-
  Quote = "\"",
  append([ Quote, Arg, Quote, After ], Before).


command_arg(Arg, Before, After) :-
  % TODO look to make sure there are no spaces in arg
  append(Arg, After, Before).

command_args([Arg | Args]) -->
  command_arg(Arg),
  ( { Args = [] }
  ; " ", command_args(Args) ).


command_name(Command, Codes) :-
  clause(evaluate(Command, _, _, _), _),
  atom_codes(Command, Codes).


%
% Definition of commands
%
evaluate(position, [FENString], _, P) :-
  fen:position(P, FENString, []).

evaluate(move, [MoveString | Moves], P, P2) :-
  pgn:pgn_string(Move, MoveString),
  
  pgn:full_move(P, Move, FullMove),
  pgn:pgn_string(FullMove, Str),
  format("~s\n", [Str]), % TODO plug in different notations here
  position:position_after(FullMove, P, P1),
  evaluate(move, Moves, P1, P2).

evaluate(move, [], P, P).

evaluate(abort, [], P, P) :-
  throw(aborted).


%
% The REPL
%
repl :-
  fen:initial_position(P),
  repl(P, []).

repl(A, B) :-
  catch(
    inner_repl(A, B),
    aborted,
    true).

inner_repl(Position, Transcript) :-
  fen:string(Position, Str),
  format("Position: ~s\n", [Str]),
  format("> "),
  read_line_to_codes(user_input, Line),

  command(Command, Arg, Line, []),

  evaluate(Command, Arg, Position, Position2), !,
  inner_repl(Position2, [[Command, Arg] | Transcript]).

inner_repl(Position, Transcript) :-
  !,
  format("Error! Something went wrong. \n"),
  repl(Position, Transcript).

