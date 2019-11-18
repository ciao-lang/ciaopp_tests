:- module(_, [q/1], []).

q(X) :-
    r(X),
    p(X).

:- use_module(m3, [r/1]).
:- use_module(m1, [p/1]).
