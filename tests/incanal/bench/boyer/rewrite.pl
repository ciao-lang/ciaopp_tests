:- module(rewrite,[rewrite/2],[]).

:- use_module(rewrite_args, [rewrite_args/3]).
:- use_module(equal, [equal/2]).

rewrite(Atom,Atom) :-
    term_typing:atomic(Atom),
    !.
rewrite(Old,New) :-
    term_basic:functor(Old,F,N),
    term_basic:functor(Mid,F,N),
    rewrite_args(N,Old,Mid),
    'rewrite/2/2/$disj/1'(New,Mid),
    !.

'rewrite/2/2/$disj/1'(New,Mid) :-
    equal(Mid,Next),
    rewrite(Next,New).
'rewrite/2/2/$disj/1'(New,Mid) :-
    term_basic:(New=Mid).

