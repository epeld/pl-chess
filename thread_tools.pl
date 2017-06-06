:- module(thread_tools, []).

%
% Thread helpers
%
cleanup_threads :-
  member(Status, [false, true, exception(_1), exited(_2)]),
  foreach(thread_property(T, status(Status)),
          thread_join(T, Status)),
  fail.

list_threads(Threads) :-
  bagof([T,S], thread_property(T, status(S)), Threads).

list_my_threads(Threads) :-
  list_threads(Threads),
  exclude(Threads, system_thread).


system_thread(main).
system_thread(pce).



create_piped_process(Path, PID, In, Out) :-
  process_create(Path, [],
                 [ stdout(pipe(Out)),
                   stdin(pipe(In)),
                   process(PID)
                 ]).
