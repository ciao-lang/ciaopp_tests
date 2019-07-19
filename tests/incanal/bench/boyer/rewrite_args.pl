:- module(rewrite_args,[rewrite_args/3],[assertions,nativeprops]).

:- use_module(rewrite, [rewrite/2]).

%:- entry rewrite_args(A,B,C) : (ground(A), indep([B,C])).

rewrite_args(0,_1,_2) :- !.
rewrite_args(N,Old,Mid) :-
        term_basic:arg(N,Old,OldArg),
        term_basic:arg(N,Mid,MidArg),
        rewrite(OldArg,MidArg),
        arithmetic:(N1 is N-1),
        rewrite_args(N1,Old,Mid).

