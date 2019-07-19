:- module(xgrun, [terminal/5, virtual/3], [assertions]).

/*
:- mode terminal(?,+,?,+,?),
        gap(+),
        virtual(+,+,?).
*/

:- trust calls terminal(_,X,_,Y,_) : (gnd(X), gnd(Y)).
:- trust calls gap(X) : gnd(X).
:- trust calls virtual(X,Y,_) : (gnd(X), gnd(Y)).


terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

gap(x(gap,_,_,_)).
gap([]).

virtual(NT,x(_,nonterminal,NT,X),X).
