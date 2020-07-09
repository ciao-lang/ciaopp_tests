:- module(chat, [main/0], []).

:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(write), [write/1]).
:- use_module('../src/top'). % top level

main :-
   write('Hi, Chat here ...'), nl,
   go.

go:- catch(hi, control_c, go).

% :- compile('chat.decls').
