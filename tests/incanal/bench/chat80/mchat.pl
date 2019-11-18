:- module(mchat,[main/0],[]).
%:- use_package([ ]).

:- use_module(library(write), [write/1]).
:- use_module(top).             % top level

main :-
   write('Hi, Chat here ...'), nl,
   go.

%go:- catch(hi, control_c, go).
go:- hi.

% :- compile('chat.decls').


