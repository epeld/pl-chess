:- module(maybe, [maybe/2]).


maybe(_, nothing).
maybe(Functor, X) :- call(Functor, X).
