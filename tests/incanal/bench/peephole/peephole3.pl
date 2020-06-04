:- module(peephole3, [], []).

:- export(popt3/2).
popt3([],[]).
popt3([Inst|Rest],Pil) :-
    popt31(Inst,Rest,Pil).
popt3([Inst|Rest],Pil) :-
    '\\+ popt31'(Inst,Rest,Pil),
    Pil=[Inst|Pil1],
    popt3(Pil,Pil1).

'\\+ popt31'(Inst,Rest,Pil) :-
    popt31(Inst,Rest,Pil), !, fail.
'\\+ popt31'(_,_,_).

popt31(getlist(R0),[unitvar(R1),unitvar(R2)|Rest],[getlist_tvar_tvar(R0,R1,R2)|OptRest]) :-
    popt3(Rest,OptRest).
popt31(getcomma(R0),[unitvar(R1),unitvar(R2)|Rest],[getcomma_tvar_tvar(R0,R1,R2)|Op_Rest]) :-
    popt3(Rest,OptRest).

