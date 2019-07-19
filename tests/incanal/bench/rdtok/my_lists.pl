:- module(_, [mymember/2, append/3], []).

mymember(X,[X|_]).
mymember(X,[_M|L]) :- mymember(X,L).
 
append([],Y,Y).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).
