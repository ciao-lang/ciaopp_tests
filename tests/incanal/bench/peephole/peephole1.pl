:- module(_,[],[]).

:- use_module(peephole_lib).

:- export(popt1/2).
popt1([],[]).
popt1([Inst|Rest],Pil1) :-
    popt11(Inst,Rest,Pil1).
popt1([Inst|Rest],Pil1) :-
    '\\+ popt11'(Inst,Rest,Pil),
    Pil1=[Inst|Pil],
    popt1(Rest,Pil).

'\\+ popt11'(Inst,Rest,Pil) :-
    popt11(Inst,Rest,Pil), !, fail.
'\\+ popt11'(_,_,_).

popt11(puttvar(T,R),[getstr(S,R)|PilRest],[putstr(S,T)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt11(movreg(T,R),[getstr(S,R)|PilRest],OptInstList) :-
    'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[puttbreg(R)|PilRest],OptInstList) :-
    'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[addreg(R,S)|PilRest],OptInstList) :-
    'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[subreg(R,S)|PilRest],OptInstList) :-
    'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[mulreg(R,S)|PilRest],OptInstList) :-
    'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[divreg(R,S)|PilRest],OptInstList) :-
    'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
    popt1(PilRest,OptPilRest).
popt11(putpvar(V,R),[getpval(V,R)|PilRest],[putpvar(V,R)|OptPilRest]) :-
    popt1(PilRest,OptPilRest).
popt11(putpval(V,R),[getstr(Str,R)|PilRest],[getstrv(Str,V)|OptPilRest]) :-
    \+ Str = ('.',2),
    popt1(PilRest,OptPilRest).
popt11(putpvar(V,R),[getstr(Str,R)|PilRest],[putstrv(Str,V)|OptPilRest]) :-
    \+ Str = ('.',2),
    popt1a(PilRest,OptPilRest).
popt11(gettval(R,R),PRest,OptPRest) :-
    popt1(PRest,OptPRest).
popt11(movreg(R,R),PRest,OptPRest) :-
    popt1(PRest,OptPRest).
popt11(jump(L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jump(Addr),[jump(_N)|PRest],[jump(Addr)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumpz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumpnz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumplt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumple(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumpgt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).
popt11(jumpge(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1(PRest,OptPRest).

'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[getstr(S,T)|OptPilRest].
'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),getstr(S,R)|OptPilRest].

'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[puttbreg(T)|OptPilRest].
'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),puttbreg(R)|OptPilRest].

'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[addreg(T,S)|OptPilRest].
'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),addreg(R,S)|OptPilRest].

'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[subreg(T,S)|OptPilRest].
'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),subreg(R,S)|OptPilRest].

'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[mulreg(T,S)|OptPilRest].
'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),mulreg(R,S)|OptPilRest].

'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    peep_chk(PilRest,R),
    OptInstList=[divreg(T,S)|OptPilRest].
'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
    '\\+ peep_chk'(PilRest,R),
    OptInstList=[movreg(T,R),divreg(R,S)|OptPilRest].

'\\+ peep_chk'(PilRest,R) :-
    peep_chk(PilRest,R), !, fail.
'\\+ peep_chk'(_,_).

popt1a([],[]).
popt1a([Inst|PilRest],Pil1) :-
    popt1a1(Inst,PilRest,Pil1).
popt1a([Inst|PilRest],Pil1) :-
    '\\+ popt1a1'(Inst,PilRest,Pil1),
    Pil1=[Inst|Pil1Rest],
    popt1a(PilRest,Pil1Rest).

'\\+ popt1a1'(Inst,PilRest,Pil1) :-
    popt1a1(Inst,PilRest,Pil1), !, fail.
'\\+ popt1a1'(Inst,PilRest,Pil1).

% :- export(popt1a1/3)
popt1a1(unipvar(X),PilRest,[bldpvar(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(unipval(X),PilRest,[bldpval(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(unitvar(X),PilRest,[bldtvar(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(unitval(X),PilRest,[bldtval(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(unicon(X),PilRest,[bldcon(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(uninumcon(X),PilRest,[bldnumcon(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(unifloatcon(X),PilRest,[bldfloatcon(X)|OptPilRest]) :-
    popt1a(PilRest,OptPilRest).
popt1a1(gettval(R,R),PRest,OptPRest) :-
    popt1a(PRest,OptPRest).
popt1a1(movreg(R,R),PRest,OptPRest) :-
    popt1a(PRest,OptPRest).
popt1a1(jump(L),[label(L)|PRest],[jump(_Addr)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jump(Addr),[jump(_N)|PRest],[jump(Addr)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumpz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumpnz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumplt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumple(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumpgt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
popt1a1(jumpge(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
    popt1a(PRest,OptPRest).
