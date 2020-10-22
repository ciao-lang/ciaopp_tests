:- module(name, [name/3], []).

:- use_module(engine(io_basic)).

name(N,X,Y):-
    display(N),
    name(X,Y),
    display('...ok'), nl.
