%------------------------------------------------------------------------------
%       Benchmark Program - Simplified Clause Annotator
%
%       by Manuel Hermenegildo, R. Warren, and M. Muthukumar
%       Date: October 1989
%
%       Query: go(N) - N times for clauses in data/1 to be repeated
%
%------------------------------------------------------------------------------

:- module( _ann, [analyze_all/2], [assertions,nativeprops] ).

:- use_module(cge, [make_CGE_phrase/6, find_vars/4, collect_vars/2]).
:- use_module(utility, [append/3, numbervars_2/3, un_number_vars/3, check_if_cge/2, builtin/1]).

:- use_module(library(assertions/native_props), [indep/2]).
:- use_module(library(lists), [length/2]).
:- use_module(engine(runtime_control), [statistics/2]).
%:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).
:- use_module(engine(io_basic)).

goal(Result) :-
    ( data(Clauses) ),
    analyze_all(Clauses,Result).

analyze_all([X|Y],[X1|Y1]) :-
    analyze(X,X1),
    analyze_all(Y,Y1).
analyze_all([],[]).

analyze(Source,NewSource) :-
    basiccontrol:'$metachoice'(_1),
    'analyze/2/$remote/1'(Source,NewSource,_1).

'analyze/2/$remote/1'(Source,NewSource,_1) :-
    term_basic:(Source=clause(_X,Body)),
    'analyze/2/$remote/1/3/1/$disj/1'(Source,NewSource,Body,_1).

'analyze/2/$remote/1/3/1/$disj/1'(Source,NewSource,Body,_1) :-
    term_basic:(Body=true),
    basiccontrol:'$metacut'(_1),
    term_basic:(NewSource=Source).
'analyze/2/$remote/1/3/1/$disj/1'(Source,NewSource,Body,_1) :-
    term_basic:(I=[]),
    term_basic:(G=[]),
    rewrite_3(Source,New_Source,I,G,_Y),
    un_number_vars(New_Source,NewSource,others),
    basiccontrol:'$metacut'(_1).

rewrite_3(clause(H,B),clause(H,P),I,G,Info) :-
    check_if_cge(B,Result),
    'rewrite_3/5/1/$disj/1'(I,G,Info,H,B,P,Result).

'rewrite_3/5/1/$disj/1'(I,G,Info,H,B,P,Result) :-
    term_basic:(Result=0),
    term_basic:(P=B).
'rewrite_3/5/1/$disj/1'(I,G,Info,H,B,P,Result) :-
    rewrite(clause(H,B),clause(H,P),I,G,Info).

rewrite(clause(H,B),clause(H,P),I,G,Info) :-
    numbervars_2(H,0,Lhv),
    collect_info(B,Info,Lhv,_X,_Y),
    add_annotations(Info,P,I,G),
    !.

add_annotations([],[],_1,_2).
add_annotations([I|Is],[P|Ps],Indep,Gnd) :-
    !,
    add_annotations(I,P,Indep,Gnd),
    add_annotations(Is,Ps,Indep,Gnd).
add_annotations(Info,Phrase,I,G) :-
    basiccontrol:'$metachoice'(_1),
    'add_annotations/4/$remote/2'(Info,Phrase,I,G,_1).

'add_annotations/4/$remote/2'(Info,Phrase,I,G,_1) :-
    !,
    para_phrase(Info,Code,Type,Vars,I,G),
    make_CGE_phrase(Type,Code,Vars,PCode,I,G),
    'add_annotations/4/$remote/2/5/1/$disj/1'(Phrase,Code,Vars,PCode,_1).

'add_annotations/4/$remote/2/5/1/$disj/1'(Phrase,Code,Vars,PCode,_1) :-
    term_typing:var(Code),
    basiccontrol:'$metacut'(_1),
    term_basic:(Phrase=PCode).
'add_annotations/4/$remote/2/5/1/$disj/1'(Phrase,Code,Vars,PCode,_1) :-
    term_basic:(Vars=[]),
    basiccontrol:'$metacut'(_1),
    term_basic:(Phrase=Code).
'add_annotations/4/$remote/2/5/1/$disj/1'(Phrase,Code,Vars,PCode,_1) :-
    term_basic:(Phrase=(PCode,Code)).

collect_info((A;B),([],sequential,(A;B)),Cin,Cout,_X) :-
    !,
    collect_info(A,_Y,Cin,C,_Z),
    collect_info(B,_N,C,Cout,_M).
collect_info((A,B),and(Ia,Ib),Cin,Cout,_X) :-
    !,
    collect_info(A,Ia,Cin,C,_Y),
    collect_info(B,Ib,C,Cout,_Z).
collect_info(A,(NewVars,CInfo,A),In,Out,_R) :-
    basiccontrol:'$metachoice'(_1),
    'collect_info/5/$remote/3'(A,(NewVars,CInfo,A),In,Out,_R,_1).

'collect_info/5/$remote/3'(A,(NewVars,CInfo,A),In,Out,_R,_1) :-
    find_vars(A,NewVars,In,Out),
    'collect_info/5/$remote/3/6/1/$disj/1'(A,NewVars,CInfo,_1),
    !.

'collect_info/5/$remote/3/6/1/$disj/1'(A,NewVars,CInfo,_1) :-
    term_basic:functor(A,F,Arity),
    builtin(F/Arity),
    basiccontrol:'$metacut'(_1),
    term_basic:(CInfo=sequential).
'collect_info/5/$remote/3/6/1/$disj/1'(A,NewVars,CInfo,_1) :-
    term_compare:(NewVars\==[]),
    basiccontrol:'$metacut'(_1),
    term_basic:(CInfo=suspect).
'collect_info/5/$remote/3/6/1/$disj/1'(A,NewVars,CInfo,_1).

para_phrase(and(X,Y),Conjuncts,Type,BagofVars,I,G) :-
    basiccontrol:'$metachoice'(_1),
    'para_phrase/6/$remote/4'(and(X,Y),Conjuncts,Type,BagofVars,I,G,_1).
para_phrase((N,T,Term),This_term,Type,Vars,_X,_Y) :-
    basiccontrol:'$metachoice'(_1),
    'para_phrase/6/$remote/4'((N,T,Term),This_term,Type,Vars,_X,_Y,_1).

'para_phrase/6/$remote/4'(and(X,Y),Conjuncts,Type,BagofVars,I,G,_1) :-
    !,
    para_phrase(X,Xcode,Xtype,XVars,I,G),
    para_phrase(Y,Ycode,Ytype,YVars,I,G),
    'para_phrase/6/$remote/4/7/1/$disj/1'(Conjuncts,Type,BagofVars,I,G,Xcode,Xtype,XVars,Ycode,Ytype,YVars,_1).
'para_phrase/6/$remote/4'((N,T,Term),This_term,Type,Vars,_X,_Y,_1) :-
    term_compare:(T==sequential),
    basiccontrol:'$metacut'(_1),
    term_basic:(Type=T),
    term_basic:(Vars=[]),
    term_basic:(This_term=Term).
'para_phrase/6/$remote/4'((N,T,Term),This_term,Type,Vars,_X,_Y,_1) :-
    collect_vars(Term,Vs),
    term_basic:(Type=par),
    term_basic:(Vars=[((T,N,Term),Vs)]).

'para_phrase/6/$remote/4/7/1/$disj/1'(Conjuncts,Type,BagofVars,I,G,Xcode,Xtype,XVars,Ycode,Ytype,YVars,_1) :-
    term_basic:(Xtype=sequential),
    basiccontrol:'$metacut'(_1),
    make_CGE_phrase(Ytype,Ycode,YVars,CGE,I,G),
    'para_phrase/6/$remote/4/7/1/$disj/1/12/1/$disj/1'(Conjuncts,Xcode,Ycode,YVars,_1,CGE),
    term_basic:(BagofVars=[]),
    term_basic:(Type=sequential).
'para_phrase/6/$remote/4/7/1/$disj/1'(Conjuncts,Type,BagofVars,I,G,Xcode,Xtype,XVars,Ycode,Ytype,YVars,_1) :-
    term_basic:(Ytype=sequential),
    basiccontrol:'$metacut'(_1),
    term_basic:(Conjuncts=Ycode),
    term_basic:(BagofVars=XVars),
    term_basic:(Type=Xtype).
'para_phrase/6/$remote/4/7/1/$disj/1'(Conjuncts,Type,BagofVars,I,G,Xcode,Xtype,XVars,Ycode,Ytype,YVars,_1) :-
    term_basic:(Conjuncts=Ycode),
    append(XVars,YVars,BagofVars),
    term_basic:(Type=Xtype).

'para_phrase/6/$remote/4/7/1/$disj/1/12/1/$disj/1'(Conjuncts,Xcode,Ycode,YVars,_1,CGE) :-
    term_typing:var(Ycode),
    basiccontrol:'$metacut'(_1),
    term_basic:(Conjuncts=(Xcode,CGE)).
'para_phrase/6/$remote/4/7/1/$disj/1/12/1/$disj/1'(Conjuncts,Xcode,Ycode,YVars,_1,CGE) :-
    term_basic:(YVars=[]),
    basiccontrol:'$metacut'(_1),
    term_basic:(Conjuncts=(Xcode,Ycode)).
'para_phrase/6/$remote/4/7/1/$disj/1/12/1/$disj/1'(Conjuncts,Xcode,Ycode,YVars,_1,CGE) :-
    term_basic:(Conjuncts=(Xcode,CGE,Ycode)).

go(N,Result) :-
    prepare(N,ManyClauses),
    analyze_all(ManyClauses,Result),
    conclude(Result).

time(A) :-
    statistics(runtime,[_N,A]).

prepare(N,ManyClauses) :-
    ( data(Clauses) ),
    mlist(N,Clauses,ManyClauses),
    time(_N).

conclude(Result) :-
    time(T),
    display('Executed in '),
    display(T),
    display(' mS.'),
    nl,
    lists:length(Result,L),
    display('Length = '),
    display(L),
    nl.

mlist(0,_1,[]).
mlist(X,SList,LList) :-
    arithmetic:(Y is X-1),
    mlist(Y,SList,MList),
    term_basic:copy_term(SList,NewSlist),
    append(NewSlist,MList,LList).

data([clause(f(X,Z),(p(X,Y),q(Y,Z))),clause(f(X1,Y1,Z1),(p(X1,Y1),q(Y1,Z1))),clause(f(X2,Y2,Z2),(Y2 is X2+Z2,p(X2,Y2),r(Y2),q(Y2,Z2))),clause(f(X3,Y3,Z3),(var(X3),f(X3),q(Y3),r(X3,Y3,Z3),q(X3))),clause(f(X4,Y4,Z4),(r(X4,Y4),p(X4,Y4),p(W4),s(X4),q(Y4,W4,Z4)))]).



