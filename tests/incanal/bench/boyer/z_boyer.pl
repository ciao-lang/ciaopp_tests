/*-----------------------------------------------------------------------------
Program: Boyer (toy theorem prover)
Author:  E. Tick (after Lisp by R. Boyer)
Date:    November 12 1985
-----------------------------------------------------------------------------*/

:- module( _boyer, [tautology/1], [assertions,nativeprops] ).

:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(rewrite, [rewrite/2]).

goal :-
        wff(X),
        tautology(X).

:- push_prolog_flag(multi_arity_warnings,off).

:- entry tautology(X)
         : ( term_typing:ground(X) ).

tautology(Wff) :-
        rewrite(Wff,NewWff),
        tautology(NewWff,[],[]).

tautology(Wff,Tlist,Flist) :-
        'tautology/3/1/$disj/1'(Wff,Tlist,Flist),
        !.

'tautology/3/1/$disj/1'(Wff,Tlist,Flist) :-
        truep(Wff,Tlist),
        !,
        true.
'tautology/3/1/$disj/1'(Wff,Tlist,Flist) :-
        falsep(Wff,Flist),
        !,
        basiccontrol:fail.
'tautology/3/1/$disj/1'(Wff,Tlist,Flist) :-
        term_basic:(Wff=if(If,Then,Else)),
        !,
        'tautology/3/1/$disj/1/3/3/$disj/1'(Tlist,Flist,If,Then,Else).

'tautology/3/1/$disj/1/3/3/$disj/1'(Tlist,Flist,If,Then,Else) :-
        truep(If,Tlist),
        !,
        tautology(Then,Tlist,Flist).
'tautology/3/1/$disj/1/3/3/$disj/1'(Tlist,Flist,If,Then,Else) :-
        falsep(If,Flist),
        !,
        tautology(Else,Tlist,Flist).
'tautology/3/1/$disj/1/3/3/$disj/1'(Tlist,Flist,If,Then,Else) :-
        tautology(Then,[If|Tlist],Flist),
        tautology(Else,Tlist,[If|Flist]).

:- pop_prolog_flag(multi_arity_warnings).

truep(t,_1) :- !.
truep(Wff,Tlist) :-
        mymember(Wff,Tlist).

falsep(f,_1) :- !.
falsep(Wff,Flist) :-
        mymember(Wff,Flist).

mymember(X,[X|_1]) :- !.
mymember(X,[_1|T]) :-
        mymember(X,T).

% ---------------

go(T) :-
        time(_1),
        wff(X),
        tautology(X),
        time(T).

time(T) :-
        statistics(runtime,[_1,T]).

wff(implies(and(implies(X,Y),and(implies(Y,Z),and(implies(Z,U),implies(U,W)))),implies(X,W))) :-
        term_basic:(X=f(plus(plus(a,b),plus(c,zero)))),
        term_basic:(Y=f(times(times(a,b),plus(c,d)))),
        term_basic:(Z=f(reverse(append(append(a,b),[])))),
        term_basic:(U=equal(plus(a,b),difference(x,y))),
        term_basic:(W=lessp(remainder(a,b),member(a,length(b)))).



