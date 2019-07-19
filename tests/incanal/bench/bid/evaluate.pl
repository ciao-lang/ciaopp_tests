:- module(evaluate,[evaluate/3],[]).

:- use_module(hcp, [hcp/3]).

evaluate(Hand,[],P) :-
    hcp(Hand,0,HCP) ,
    adjustments(Hand,MP) ,
    P is HCP + MP.

adjustments(_,0).

