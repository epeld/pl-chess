:- module(engine_client, []).

engine_alias(engine).

create(Id) :-
  engine_alias(Alias),
  engine:create_thread(Id, Alias).

%
% The complete engine protocol:
%
async(quit).
async(go(_Position)).
async(go(_Position, _Options)).
async(stop).
async(setoption(_Name, _Value)).

sync(state(_State)).
sync(ping).


%
% Public interface:
%
engine_call(Functor) :-
  async(Functor),
  !,
  call_async(Functor).

engine_call(Functor) :-
  sync(Functor),
  functor(Functor, Atom, 1),
  !,
  call_sync(Atom, Functor).

engine_call(Functor) :-
  sync(Functor),
  functor(Functor, Atom, 0),
  !,
  call_sync(Atom, ok).

%
% Convenience
%
engine_analysis(Analysis) :-
  engine_call(state(State)),
  engine_state:engine_analysis(State, Analysis).

engine_status(Status) :-
  engine_call(state(State)),
  functor(State, Status, _).

engine_name(Name) :-
  engine_call(state(State)),
  engine_state:engine_id(State, Ids),
  member(name(Name), Ids).

engine_author(Name) :-
  engine_call(state(State)),
  engine_state:engine_id(State, Ids),
  member(author(Name), Ids).

engine_options(Options) :-
  engine_call(state(State)),
  engine_state:engine_options(State, Options).

engine_go(Pos) :-
  engine_call(go(Pos)).

engine_quit :-
  engine_call(quit).

%
% Helpers
%
call_sync(Request, Reply) :-
  engine_alias(Alias),
  thread_property(Alias, status(running)),
  
  thread_self(Self),
  
  thread_send_message(Alias, reply_to(Self, Request)),
  thread_get_message(Self, Reply, [timeout(3)]).

call_async(Request) :-
  engine_alias(Alias),
  thread_send_message(Alias, async(Request)).
