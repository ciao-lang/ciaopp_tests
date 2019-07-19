:- module(_,[make_CGE_phrase/6,find_vars/4,collect_vars/2],[]).
:- use_module(utility, [append/3, mymember/2, subtract/3, singleton/2, merge/3, intersect/3, numbervars_2/3]).
%----------------------------------------------
% Make a parallel phrase that has the general 
% form:
% 	gnd(), indep(),!, parcode; sequential
%----------------------------------------------
:- use_module(library(sort)).
:- use_module(library(lists), [length/2]).

make_CGE_phrase(sequential,Code,_1,Code,_2,_3) :- !.
make_CGE_phrase(_X,_Y,VarList,NewCode,I,G) :-
        get_phrase(VarList,NewCode,I,G),
        !.

get_phrase(VarList,Code,I,G) :-
        basiccontrol:'$metachoice'(_1),
        'get_phrase/4/$remote/5'(VarList,Code,I,G,_1).

'get_phrase/4/$remote/5'(VarList,Code,I,G,_1) :-
        lists:length(VarList,L),
        'get_phrase/4/$remote/5/5/1/$disj/1'(VarList,Code,I,G,L,_1).

'get_phrase/4/$remote/5/5/1/$disj/1'(VarList,Code,I,G,L,_1) :-
        arithmetic:(L>1),
        basiccontrol:'$metacut'(_1),
        eliminate_suspect_code(VarList,Rem,Vs,I,G),
        get_CGE_2(Vs,I,G,Code,Rem).
'get_phrase/4/$remote/5/5/1/$disj/1'(VarList,Code,I,G,L,_1) :-
        term_basic:(VarList=[((_X,_Y,Code),_Z)]).

eliminate_suspect_code([This_goal,Last_goal],SeqCode,ParCode,_X,_Y) :-
        basiccontrol:'$metachoice'(_1),
        'eliminate_suspect_code/5/$remote/6'([This_goal,Last_goal],SeqCode,ParCode,_X,_Y,_1).
eliminate_suspect_code([This_goal|Subsequent_goals],Code,ParCode,I,G) :-
        basiccontrol:'$metachoice'(_1),
        'eliminate_suspect_code/5/$remote/6'([This_goal|Subsequent_goals],Code,ParCode,I,G,_1).

'eliminate_suspect_code/5/$remote/6'([This_goal,Last_goal],SeqCode,ParCode,_X,_Y,_1) :-
        !,
        term_basic:(This_goal=((suspect,FVars,_Z),_W)),
        'eliminate_suspect_code/5/$remote/6/6/1/$disj/1'(SeqCode,ParCode,This_goal,Last_goal,FVars,_1).
'eliminate_suspect_code/5/$remote/6'([This_goal|Subsequent_goals],Code,ParCode,I,G,_1) :-
        term_basic:(This_goal=((suspect,FVars,_X),_Y)),
        eliminate_suspect_code(Subsequent_goals,SCode,PCode,I,G),
        'eliminate_suspect_code/5/$remote/6/6/2/$disj/1'(Code,ParCode,I,G,This_goal,FVars,SCode,PCode,_1).

'eliminate_suspect_code/5/$remote/6/6/1/$disj/1'(SeqCode,ParCode,This_goal,Last_goal,FVars,_1) :-
        found(FVars,[Last_goal]),
        basiccontrol:'$metacut'(_1),
        term_basic:(Last_goal=((_L,_M,SeqCode),_N)),
        term_basic:(ParCode=[This_goal]).
'eliminate_suspect_code/5/$remote/6/6/1/$disj/1'(SeqCode,ParCode,This_goal,Last_goal,FVars,_1) :-
        term_basic:(ParCode=[This_goal,Last_goal]).

'eliminate_suspect_code/5/$remote/6/6/2/$disj/1'(Code,ParCode,I,G,This_goal,FVars,SCode,PCode,_1) :-
        found(FVars,PCode),
        basiccontrol:'$metacut'(_1),
        get_CGE_2(PCode,I,G,Code,SCode),
        term_basic:(ParCode=[This_goal]).
'eliminate_suspect_code/5/$remote/6/6/2/$disj/1'(Code,ParCode,I,G,This_goal,FVars,SCode,PCode,_1) :-
        term_basic:(ParCode=[This_goal|PCode]),
        term_basic:(Code=SCode).

found([],_R) :-
        basiccontrol:fail.
found([H|T],Next_calls) :-
        basiccontrol:'$metachoice'(_1),
        'found/2/$remote/7'([H|T],Next_calls,_1).

'found/2/$remote/7'([H|T],Next_calls,_1) :-
        find_var(Next_calls,H),
        basiccontrol:'$metacut'(_1).
'found/2/$remote/7'([H|T],Next_calls,_1) :-
        found(T,Next_calls).

find_var([],_R) :-
        basiccontrol:fail.
find_var([(_R,NextCall)|Others],V) :-
        basiccontrol:'$metachoice'(_1),
        'find_var/2/$remote/8'([(_R,NextCall)|Others],V,_1).

'find_var/2/$remote/8'([(_R,NextCall)|Others],V,_1) :-
        mymember(V,NextCall),
        basiccontrol:'$metacut'(_1).
'find_var/2/$remote/8'([(_R,NextCall)|Others],V,_1) :-
        find_var(Others,V).

get_CGE_2([((_X,_Y,Goal),_Z)],_W,_M,Goal,Others) :-
        term_typing:var(Others),
        !.
get_CGE_2([((_4,_5,Goal),_3)],_1,_2,(Goal,Others),Others) :- !.
get_CGE_2(Vs,I,G,Code,Rem) :-
        get_conds(Vs,Conds,I,G),
        make_norml_2(Vs,Parallel),
        babel_2(Conds,Parallel,Rem,Code).

get_conds(VARS,Y,I,G) :-
        do_intersect(VARS,GS),
        subtract(GS,G,Gnd),
        singleton(VARS,VOIDS),
        reduce_set(VARS,Gnd,VOIDS,I,RSET),
        produce_tuples(RSET,Indep),
        get_text(Gnd,Indep,Y).

'$simplify_unconditional_cges'(off).

babel_2(Conds,Parallel,C,Code) :-
        basiccontrol:'$metachoice'(_1),
        'babel_2/4/$remote/9'(Conds,Parallel,C,Code,_1).

'babel_2/4/$remote/9'(Conds,Parallel,C,Code,_1) :-
        'babel_2/4/$remote/9/5/1/$disj/1'(Conds,Parallel,Pcode,_1),
        'babel_2/4/$remote/9/5/1/$disj/2'(C,Code,Pcode,_1).

'babel_2/4/$remote/9/5/1/$disj/1'(Conds,Parallel,Pcode,_1) :-
        term_basic:(Conds=true),
        '$simplify_unconditional_cges'(on),
        basiccontrol:'$metacut'(_1),
        term_basic:(Pcode=Parallel).
'babel_2/4/$remote/9/5/1/$disj/1'(Conds,Parallel,Pcode,_1) :-
        term_basic:(Pcode=..[=>,Conds,Parallel]).

'babel_2/4/$remote/9/5/1/$disj/2'(C,Code,Pcode,_1) :-
        term_typing:var(C),
        basiccontrol:'$metacut'(_1),
        term_basic:(Code=Pcode).
'babel_2/4/$remote/9/5/1/$disj/2'(C,Code,Pcode,_1) :-
        term_basic:(Code=(Pcode,C)).

reduce_set([],_X,_Y,_Z,[]).
reduce_set([(Info,V)|Vs],Gnd,VOIDS,Indep,[(Info,Rset)|Rs]) :-
        !,
        reduce_set(Vs,Gnd,VOIDS,Indep,Rs),
        subtract(V,Gnd,Rg),
        subtract(Rg,VOIDS,Rv),
        subtract(Rv,Indep,Rset).

produce_tuples([],[]).
produce_tuples([(_N,V)|Vs],Tuples) :-
        !,
        produce_tuples(Vs,Ts),
        pair(Vs,V,Ps),
        merge(Ps,Ts,Tuples).

pair([],_1,[]).
pair([(_N,L)|Ls],Vs,Tuples) :-
        pair(Ls,Vs,Ps),
        tuple(L,Vs,Ts),
        merge(Ts,Ps,Tuples).

tuple([],_1,[]).
tuple(_1,[],[]).
tuple(List,[L|Ls],Tuples) :-
        !,
        tuple(List,Ls,T1),
        tuple(List,L,T2),
        merge(T1,T2,Tuples).
tuple([E|Es],L,Tuples) :-
        basiccontrol:'$metachoice'(_1),
        'tuple/3/$remote/10'([E|Es],L,Tuples,_1).

'tuple/3/$remote/10'([E|Es],L,Tuples,_1) :-
        tuple(Es,L,T1),
        !,
        'tuple/3/$remote/10/4/1/$disj/1'(L,E,Pair,_1),
        merge(Pair,T1,Tuples).

'tuple/3/$remote/10/4/1/$disj/1'(L,E,Pair,_1) :-
        term_compare:(E@<L),
        basiccontrol:'$metacut'(_1),
        term_basic:(Pair=[[E,L]]).
'tuple/3/$remote/10/4/1/$disj/1'(L,E,Pair,_1) :-
        term_basic:(Pair=[[L,E]]).

make_norml([((_2,_3,T),_1)],T) :- !.
make_norml([((_X,_Y,T),_Z)|Nxt],(T,Nc)) :-
        make_norml(Nxt,Nc).

make_norml_2([((_2,_3,T),_1)],T) :- !.
make_norml_2([((_X,_Y,T),_Z)|Nxt],&(T,Nc)) :-
        make_norml_2(Nxt,Nc).

do_intersect([],[]).
do_intersect([(_X,S1),(_Y,S2)],IS) :-
        !,
        intersect(S1,S2,IS).
do_intersect([S1,S2|Ss],IS) :-
        do_intersect([S2|Ss],IS1),
        do_intersect([S1|Ss],IS2),
        do_intersect([S1,S2],IS3),
        merge(IS1,IS2,T1),
        merge(IS3,T1,IS).

get_text([],[],true).
get_text([],X,indep(X)).
get_text(X,[],ground(X)).
get_text(X,Y,(ground(X),indep(Y))).

find_vars(T,Vars,Cin,Cout) :-
        numbervars_2(T,Cin,Cout),
        arithmetic:(NewVars is Cout-Cin),
        lists:length(Vars,NewVars),
        numbervars_2(Vars,Cin,_N),
        !.

collect_vars(X,[X]) :-
        term_typing:var(X),
        !.
collect_vars('$VAR'(X),['$VAR'(X)]) :- !.
collect_vars('$VAR'(X,Y),['$VAR'(X,Y)]) :- !.
collect_vars(Term,Vars) :-
        term_basic:functor(Term,_N,A),
        go_inside(A,Term,Vs),
        sort:sort(Vs,Vars).
collect_vars(_1,[]).

go_inside(0,_1,[]) :- !.
go_inside(N,T,Bag) :-
        basiccontrol:'$metachoice'(_1),
        'go_inside/3/$remote/11'(N,T,Bag,_1).

'go_inside/3/$remote/11'(N,T,Bag,_1) :-
        arithmetic:(Nth is N-1),
        go_inside(Nth,T,C),
        term_basic:arg(N,T,ARG),
        'go_inside/3/$remote/11/4/1/$disj/1'(Bag,C,ARG,_1).

'go_inside/3/$remote/11/4/1/$disj/1'(Bag,C,ARG,_1) :-
        term_typing:var(ARG),
        basiccontrol:'$metacut'(_1),
        term_basic:(Bag=[ARG|C]).
'go_inside/3/$remote/11/4/1/$disj/1'(Bag,C,ARG,_1) :-
        term_typing:atomic(ARG),
        basiccontrol:'$metacut'(_1),
        term_basic:(Bag=C).
'go_inside/3/$remote/11/4/1/$disj/1'(Bag,C,ARG,_1) :-
        collect_vars(ARG,Cs),
        append(Cs,C,Bag).

