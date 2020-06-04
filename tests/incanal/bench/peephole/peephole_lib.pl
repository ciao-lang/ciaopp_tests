:- module(_,[],[]).

:- export(popt_chkmember/3).
popt_chkmember(P,L,Flag) :-
    var(L),
    L=[P|_N],
    Flag=0.
popt_chkmember(P,L,Flag) :-
    nonvar(L),
    L=[P1|L1],
    'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1).

'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1) :-
    P=P1,
    !,
    Flag=1.
'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1) :-
    popt_chkmember(P,L1,Flag).

% -----------------------------------------------------------

:- export(member1/2).
member1(X,[X|_Xs]).
member1(X,[_N|Xs]) :-
    member1(X,Xs).

:- export(peep_chk/2).
peep_chk([],_1).
peep_chk([Inst|Rest],R) :-
    '\\+ peep_use'(Inst,R),
    term_or_chk([Inst|Rest],R).

'\\+ peep_use'(Inst,R) :-
    peep_use(Inst,R), !, fail.
'\\+ peep_use'(_,_).

:- export(term_or_chk/2).
term_or_chk([Inst|_N],R) :-
    peep_term(Inst,R),
    !.
term_or_chk([_N|Rest],R) :-
    peep_chk(Rest,R).

% -----------------------------------------------------------

:- export(peep_use/2).
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

:- export(peep_term/2).
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
