:- module(test,[test/1],[]).

%:- use_package(assertions).

:- use_module(qsort).
:- use_module(mylists).
%:- pred test(X) : ground(X).

test(L):-  length(L,Length), length(Result,Length),
           qsort(L,Result), sorted(Result).

sorted([]).
sorted([_]).
sorted([X,Y|Z]):-  X =< Y, sorted([Y|Z]).
