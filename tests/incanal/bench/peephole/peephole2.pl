:- module(peephole2, [],[]).

:- use_module(peephole_lib).

% -----------------------------------------------------------
:- export(popt2/2).
popt2([],[]).
popt2([Inst|PilRest],Pil1) :-
    popt21(Inst,PilRest,Pil1).
popt2([Inst|PilRest],Pil1) :-
    '\\+ popt21'(Inst,PilRest,Pil1),
    Pil1=[Inst|Pil1Rest],
    popt2(PilRest,Pil1Rest).

'\\+ popt21'(Inst,PilRest,Pil1) :-
    popt21(Inst,PilRest,Pil1), !, fail.
'\\+ popt21'(_,_,_).

popt21(getstr(('.',2),R),PilRest,[getlist(R)|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
popt21(putstr(('.',2),R),PilRest,[putlist(R)|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
popt21(getcon([],R),PilRest,[getnil(R)|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
popt21(putcon([],R),PilRest,[putnil(R)|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
popt21(unicon([]),PilRest,[uninil|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
popt21(bldcon([]),PilRest,[bldnil|OptPilRest]) :-
    popt2(PilRest,OptPilRest).
