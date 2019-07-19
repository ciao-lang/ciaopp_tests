:- module(_, [p/1, main/2], []).

:- use_module(simple2, [q/1]).

p(a).
p(b).
p(c).
p(d).
p(e).
p(f).
p(X) :- q(X).

main(X, Y) :-
	q(X),
	p(Y).