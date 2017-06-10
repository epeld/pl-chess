:- module(engine_client, []).

engine_alias(engine).

create(Id) :-
  engine_alias(Alias),
  engine:create_thread(Id, Alias).

%
% The complete engine protocol:
%
async(quit).
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
  call_async(Functor).

engine_call(Functor) :-
  sync(Functor),
  functor(Atom, Functor, 1),
  call_sync(Atom, Functor).

engine_call(Functor) :-
  sync(Functor),
  functor(Atom, Functor, 0),
  call_sync(Atom, ok).


%
% Helpers
%
call_sync(Request, Reply) :-
  engine_alias(Alias),
  thread_property(Alias, status(running)),
  
  thread_self(Self),
  
  thread_send_message(Alias, reply_to(Self, Request)),
  thread_get_message(Reply, [timeout(3)]).

call_async(Request) :-
  engine_alias(Alias),
  thread_send_message(Alias, async(Request)).
