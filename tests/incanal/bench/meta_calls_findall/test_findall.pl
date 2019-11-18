:- module(_, [test/2], []).

:- use_module(library(aggregates), [findall/3]).

test(A, L) :-
    findall(A, handler(A), L).

handler(a).
handler(_).
