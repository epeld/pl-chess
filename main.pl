
:- module(main, []).

:- dynamic(known_position/2).
:- dynamic(valid_move/2).
:- dynamic(invalid_move/2).

:- set_prolog_flag(double_quotes, codes).

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
% State-mutating commands
%

evaluate(comment, [], State, State) :-
  state:state_comment(State, Comment),
  format("\"~s\"\n", [Comment]).

evaluate(comment, [Comment], State, State2) :-
  state:state_parts(State, P, M, _),
  state:state_parts(State2, P, M, Comment),
  format("OK\n").

evaluate(truncate, [], State, State2) :-
  state:state_truncate(State, State2).

evaluate(forward, [], State, State2) :-
  state:state_forward(State, State2).

evaluate(back, [], State, State2) :-
  state:state_forward(State2, State).

evaluate(initial, [], _, State) :-
  state:initial_state(State).

evaluate(position, [FENString], _, State) :-
  fen:position(P, FENString, []),
  state:custom_state(State, P).

evaluate(position, [Codes], _, State) :-
  atom_codes(Atom, Codes),
  known_position(Atom, FEN),
  fen:string(P, FEN),
  state:custom_state(State, P).

evaluate(move, MoveStrings, State, State2) :-
  maplist(pgn:pgn_string, Moves, MoveStrings),
  state:state_move(Moves, State, State2).


%
%  "REPL"-control
%
evaluate(abort, [], P, P) :-
  throw(aborted).


%
%  Debugging Predicates
%
evaluate(save, [], P, P) :-
  save_predicates.

evaluate(load, [], P, P) :-
  load_predicates.

evaluate(store, [Name], State, State) :-
  state:state_position(State, P),
  atom_codes(Atom, Name),
  fen:string(P, FEN),
  format("Storing position ~s as ~s\n", [FEN, Name]),

  (retract(known_position(Atom, _)) ; true),
  assertz(known_position(Atom, FEN)).

evaluate(valid_move, [MoveString], State, State) :-
  state:state_position(State, P),
  fen:string(P, FENString),
  format("Generating a Test Case\n"),
  format("Encoding that in the position: \n\t~s\nthe move ~s should work.\n\n",
         [FENString, MoveString]),

  assertz(valid_move(FENString, MoveString)),
  save_predicates.

evaluate(invalid_move, [MoveString], State, State) :-
  state:state_position(State, P),
  fen:string(P, FENString),
  format("Generating a Test Case\n"),
  format("Encoding that in the position: \n\t~s\nthe move ~s should NOT work.\n\n",
         [FENString, MoveString]),

  assertz(invalid_move(FENString, MoveString)),
  save_predicates.



%
% The REPL
%
repl :-
  state:initial_state(State),
  repl(State).

repl(State) :-
  catch(
    inner_repl(State),
    aborted,
    true).

% Yes, I guess it should be called PREL instead of REPL
inner_repl(State) :-

  % Print
  state:state_position(State, Position),
  print_position(Position),

  % Read:
  read_command(Command, Args),

  % Eval:
  evaluate(Command, Args, State, State2),

  % Loop:
  !, inner_repl(State2).

inner_repl(State) :-
  !,
  format("Error! Something went wrong. \n---\n"),
  repl(State).


print_position(Position) :-
  fen:string(Position, Str),
  format("~s\n", [Str]).



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


%
% Persistence
%

settings_file(Atom) :-
  atom_codes(Atom, "settings.pl").

pretty_comment(Message) :-
  format("\n\n%%%%\n% ~s \n%%%%\n\n", [Message]).

save_predicates :-
  settings_file(Settings),
  tell(Settings),

  pretty_comment("Known Positions"),
  forall(clause(known_position(A, B), true),
         (write(known_position(A, B)), format(".\n"))),

  pretty_comment("Valid Moves"),
  forall(clause(valid_move(A, B), true),
         (write(valid_move(A, B)), format(".\n"))),

  pretty_comment("Invalid Moves"),
  forall(clause(invalid_move(A, B), true),
         (write(invalid_move(A, B)), format(".\n"))),

  told.


load_predicates :-
  settings_file(Settings),
  exists_file(Settings),
  retractall(known_position(_, _)),
  retractall(valid_move(_, _)),
  retractall(invalid_move(_, _)),
  consult(Settings).


