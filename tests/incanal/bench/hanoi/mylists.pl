:- module(mylists,[append/3],[assertions]).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).


