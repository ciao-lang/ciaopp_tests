:- module(_, [p/1], []).

p(a).
p(b).
p(X) :-
	q(X).

:- use_module(m2, [q/1]).