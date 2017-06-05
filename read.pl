
:- module(read, []).

start_reader(Streams, Options, Id) :-
  thread_self(Self),
  thread_create(reader_main(Streams, Self), Id, Options).

reader_main(Streams, Reportee) :-
  read_lines_report_to(Streams, Reportee).

reader_main(_Streams, Reportee) :-
  thread_self(Self),
  thread_send_message(Reportee, reader_failed(Self)).

% Starts an infinite (blocking) loop,
% waiting for input on the list of streams
% and reporting any lines read to Reportee (through thread_send_message/2)
read_lines_report_to([Stream], Reportee) :-
  read_line_report_to(Stream, Reportee),
  \+ at_end_of_stream(Stream),
  read_lines_report_to([Stream], Reportee).

% at least two elements
read_lines_report_to([_1, _2 | _], Reportee) :-
  Streams = [_1, _2 | _],
  wait_for_input(Streams, Ready, infinite),
  forall(member(Stream, Ready),
         read_line_report_to(Stream, Reportee)),
  exclude(Streams2, at_end_of_stream),
  read_lines_report_to(Streams2, Reportee).


read_line_report_to(Stream, Reportee) :-
    read_line_to_codes(Stream, Line),
    thread_send_message(Reportee, line_read(Stream, Line)).
