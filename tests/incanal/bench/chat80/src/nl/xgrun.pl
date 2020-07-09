:- module(xgrun, [terminal/5, virtual/3], [assertions,isomodes]).


:- pred terminal(?,+,?,+,?).
terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

:- pred gap(+).
gap(x(gap,_,_,_)).
gap([]).

:- pred virtual(+,+,?).
virtual(NT,x(_,nonterminal,NT,X),X).
