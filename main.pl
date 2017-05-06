
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

evaluate(store, [Name], P, P) :-
  format("In the future, this would have stored the current position as ~s\n", [Name]).

evaluate(status, [], P, P) :-
  status_string(P, S),
  format("~s\n", [S]).

evaluate(initial, [], _, P) :-
  fen:initial_position(P).

evaluate(position, [FENString], _, P) :-
  fen:position(P, FENString, []).

evaluate(move, [MoveString | Moves], P, P2) :-
  pgn:pgn_string(Move, MoveString),
  
  pgn:full_move(P, Move, FullMove),
  
  pgn:pgn_string(FullMove, Str),
  pgn:legal_position_after(FullMove, P, P1),
  check_info(P1, Extra),
  
  format("~s~s\n", [Str, Extra]), % TODO plug in different notations here
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

% Yes, I guess it should be called PREL instead of REPL
inner_repl(Position, Transcript) :-

  % Print
  print_position(Position),

  % Read:
  read_command(Command, Args),

  % Eval:
  evaluate(Command, Args, Position, Position2),

  % Loop:
  !, inner_repl(Position2, [[Command, Args] | Transcript]).

inner_repl(Position, Transcript) :-
  !,
  format("Error! Something went wrong. \n---\n"),
  repl(Position, Transcript).


print_position(Position) :-
  fen:string(Position, Str),
  status_string(Position, StatusStr),
  format("~s\n~s\n", [Str, StatusStr]).



read_command(Command, Args) :-
  % Print a prompt:
  format("> "),
  
  read_line_to_codes(user_input, Line),
  identify_command(Command, Args, Line).

identify_command(Command, Args, Line) :-
  command(Command, Args, Line, []).

identify_command(Command, Args, Line) :-
  findall(Cmd, command(Cmd, _, Line, []), []),

  format("Error. Cannot interpret: \"~s\"\n", [Line]),

  % Try again:
  read_command(Command, Args).

%
% Status
%

check_info(Position, "#") :-
  pgn:checkmate(Position).

check_info(Position, "+") :-
  \+ pgn:checkmate(Position),
  pgn:check(Position, _).

check_info(Position, "") :-
  \+ pgn:check(Position, _).



status_string(Position, "MATE") :-
  pgn:checkmate(Position).

status_string(Position, "STALEMATE") :-
  pgn:stalemate(Position).

status_string(Position, "CHECK") :-
  \+ pgn:checkmate(Position),
  pgn:check(Position, _).

status_string(Position, "") :-
  \+ pgn:check(Position, _).
