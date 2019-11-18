:- module(peephole3, [popt1a1/3, peep_use/2, term_or_chk/2], []).

:- use_module(peephole2, [peep_chk/2, popt1a/2]).

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

% -----------------------------------------------------------

peep_use(getcon(_1,R),R).
peep_use(getnumcon(_1,R),R).
peep_use(getfloatcon(_1,R),R).
peep_use(getpval(_1,R),R).
peep_use(gettval(_1,R),R).
peep_use(gettval(R,_1),R).
peep_use(gettbreg(R),R).
peep_use(getpbreg(R),R).
peep_use(getstr(_1,R),R).
peep_use(getstrv(_1,R),R).
peep_use(getlist(R),R).
peep_use(getlist_tvar_tvar(R,_1,_2),R).
peep_use(getcomma(R),R).
peep_use(getcomma_tvar_tvar(R,_1,_2),R).
peep_use(unitval(R),R).
peep_use(unipval(R),R).
peep_use(bldtval(R),R).
peep_use(bldpval(R),R).
peep_use(and(R,_1),R).
peep_use(and(_1,R),R).
peep_use(negate(R),R).
peep_use(or(R,_1),R).
peep_use(or(_1,R),R).
peep_use(logshiftl(R,_1),R).
peep_use(logshiftl(_1,R),R).
peep_use(logshiftr(R,_1),R).
peep_use(logshiftr(_1,R),R).
peep_use(addreg(R,_1),R).
peep_use(addreg(_1,R),R).
peep_use(subreg(R,_1),R).
peep_use(subreg(_1,R),R).
peep_use(mulreg(R,_1),R).
peep_use(mulreg(_1,R),R).
peep_use(divreg(R,_1),R).
peep_use(divreg(_1,R),R).
peep_use(movreg(R,_1),R).
peep_use(switchonterm(R,_1,_2),R).
peep_use(switchoncon(R,_1,_2),R).
peep_use(switchonstr(R,_1,_2),R).
peep_use(switchonbound(R,_1,_2),R).
peep_use(jump(_2),_1).
peep_use(jumpeq(_2,_3),_1).
peep_use(jumpne(_2,_3),_1).
peep_use(jumplt(_2,_3),_1).
peep_use(jumple(_2,_3),_1).
peep_use(jumpgt(_2,_3),_1).
peep_use(jumpge(_2,_3),_1).

% -----------------------------------------------------------

term_or_chk([Inst|_N],R) :-
    peep_term(Inst,R),
    !.
term_or_chk([_N|Rest],R) :-
    peep_chk(Rest,R).

peep_term(call(_2,_3),_1).
peep_term(calld(_2,_3),_1).
peep_term(execute(_2),_1).
peep_term(putcon(R),R).
peep_term(putnumcon(R),R).
peep_term(putfloatcon(R),R).
peep_term(puttvar(R,_1),R).
peep_term(putpvar(_1,R),R).
peep_term(putdval(_1,R),R).
peep_term(putuval(_1,R),R).
peep_term(puttbreg(R),R).
peep_term(putpval(_1,R),R).
peep_term(putstr(_1,R),R).
peep_term(putstrv(_1,R),R).
peep_term(putlist(R),R).
peep_term(putnil(R),R).
peep_term(movreg(_1,R),R).
peep_term(bldtvar(R),R).

