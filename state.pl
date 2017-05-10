
:- module(state, []).

%
% This Module represents the editor state when viewing a PGN file.
% You can move forwards and backwards in the game history,
% as well as inspect Position, Moves and Comments along the way
%

initial_state([state, Zipper, []]) :-
  fen:initial_position(P),
  zipper:zsingleton(Zipper, [P, initial, "Starting Position"]).


custom_state([state, Zipper, []], P) :-
  \+ fen:initial_state(P),
  zipper:zsingleton(Zipper, [P, initial, "Custom Starting Position"]).

custom_state(State, P) :-
  initial_state(State),
  state_position(State, P).
  


state_parts([state, Zipper, []], Position, Move, Comment) :-
  zipper:zvalue(Zipper, [Position, Move, Comment]).


state_position(State, Position) :-
  state_parts(State, Position, _, _).

state_comment(State, Comment) :-
  state_parts(State, _, _, Comment).

state_last_move(State, Move) :-
  state_parts(State, _, Move, _).


state_move([PartialMove | Moves], State, StateN) :-
  % Extract the current position
  State = [state, Zipper, Options],
  state_position(State, Position),

  % Apply the move to it..
  pgn:full_move(Position, PartialMove, Move),
  pgn:legal_position_after(Move, Position, Position2),

  % Construct a new 'node' to put into the zipper..
  zipper:zput(Zipper, Zipper1, [[Position2, Move, ""]]),

  % and forward so that the new position is active
  zipper:zforward(Zipper1, Zipper2),

  State2 = [state, Zipper2, Options],
  state_move(Moves, State2, StateN).

state_move([], S, S).


state_forward([state, Zipper, Options], [state, Zipper2, Options]) :-
  zipper:zforward(Zipper, Zipper2).


state_fastforward([state, Zipper, Options], [state, Zipper2, Options]) :-
  zipper:zfastforward(Zipper, Zipper2).


state_rewind([state, Zipper, Options], [state, Zipper2, Options]) :-
  zipper:zrewind(Zipper, Zipper2).


state_truncate([state, Zipper, Options], [state, Zipper2, Options]) :-
  zipper:zput(Zipper2, Zipper, _).

state_replace_comment([state, Zipper, Options], [state, Zipper2, Options], Comment) :-
  zipper:zvalue(Zipper, [Position, Move, _]),
  zipper:zset(Zipper, Zipper2, [Position, Move, Comment]).
