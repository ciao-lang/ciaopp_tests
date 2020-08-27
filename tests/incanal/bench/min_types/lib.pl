:- module(_,[],[]).

:- export(append/3).
append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).
