/* from the Gabriel Benchmark */  
/* obtained from Tep Dobry */
/* modified by Herve' Touati 01/15/87 */

:- module(browse,[investigate/2],[assertions]).

:- use_module(.(browse_ops)).

investigate([],_).
investigate([U|Units],Patterns) :-
    property(U,pattern,Data),
    p_investigate(Data,Patterns),
    investigate(Units,Patterns).



property([],_X,_Y) :- fail.  /* don't really need this */
property([Prop|_RProps],P,Val) :-
    functor(Prop,P,_X),!,
    arg(1,Prop,Val).
property([_X|RProps],P,Val) :-
    property(RProps,P,Val).
