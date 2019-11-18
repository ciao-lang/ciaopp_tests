:- module(_,[append/3,numbervars_2/3,un_number_vars/3,check_if_cge/2,builtin/1
    ,mymember/2,subtract/3,singleton/2,merge/3,intersect/3],[]).
%-------------------------------------------------------------
%   Package: basics
%   Author : by now, nobody knows
%   Updated: 7/29/85
%   Defines: the basic list processing predicates

:- use_module(library(lists), [length/2]).

append([],L,L).
append([H|T],L,[H|R]) :-
    append(T,L,R).

mymember(Element,[Element|_1]).
mymember(Element,[_N|Rest]) :-
    mymember(Element,Rest).

memberchk(Element,[Element|_1]) :- !.
memberchk(Element,[_N|Rest]) :-
    memberchk(Element,Rest).

subtract([],_1,[]).
subtract([Element|Residue],Set,Difference) :-
    memberchk(Element,Set),
    !,
    subtract(Residue,Set,Difference).
subtract([Element|Residue],Set,[Element|Difference]) :-
    subtract(Residue,Set,Difference).

singleton([((_2,X,_3),_1)],X).
singleton([((_A,V,_B),_C)|Vs],X) :-
    singleton(Vs,Y),
    merge(V,Y,X).

merge([],D,D) :- !.
merge(D,[],D) :- !.
merge([A|As],[D|Ds],[A|Bs]) :-
    term_compare:(A@<D),
    !,
    merge(As,[D|Ds],Bs).
merge([A|As],[D|Ds],[A|Bs]) :-
    term_compare:(A==D),
    !,
    merge(As,Ds,Bs).
merge(As,[D|Ds],[D|Bs]) :-
    merge(As,Ds,Bs).

intersect([],_1,[]) :- !.
intersect(_1,[],[]) :- !.
intersect([A|As],[D|Ds],Out) :-
    term_compare:(A@<D),
    !,
    intersect(As,[D|Ds],Out).
intersect([A|As],[D|Ds],[A|Out]) :-
    term_compare:(A==D),
    !,
    intersect(As,Ds,Out).
intersect(As,[_N|Ds],Out) :-
    intersect(As,Ds,Out).

numbervars_2(X,N,N1) :-
    term_typing:var(X),
    arithmetic:(N1 is N+1),
    !,
    term_basic:(X='$VAR'(N,_L)).
numbervars_2(A,N,N) :-
    term_typing:atomic(A),
    !.
numbervars_2('$VAR'(_1,_2),N,N).
numbervars_2(F,N,N1) :-
    numbervars_2(0,F,N,N1).

numbervars_2(I,F,N,N1) :-
    arithmetic:(I1 is I+1),
    term_basic:arg(I1,F,X),
    numbervars_2(X,N,N0),
    !,
    numbervars_2(I1,F,N0,N1).
numbervars_2(_1,_2,N,N).

un_number_vars(clause(Head,Body),clause(H,B),X) :-
    un_number_vars_2(Head,H,X),
    un_number_vars_2(Body,B,X),
    !.
un_number_vars(directive(X),directive(X),_1).

un_number_vars_2('$VAR'(_1,X),X,others) :- !.
un_number_vars_2('$VAR'(X,_1),'$VAR'(X),cge) :- !.
un_number_vars_2(X,X,_1) :-
    term_typing:atomic(X),
    !.
un_number_vars_2(F,F1,Y) :-
    term_basic:functor(F,Func,_N),
    un_number_vars_2(0,F,List,Y),
    term_basic:(F1=..[Func|List]).

un_number_vars_2(I,F,[X1|Tail],Y) :-
    arithmetic:(I1 is I+1),
    term_basic:arg(I1,F,X),
    un_number_vars_2(X,X1,Y),
    !,
    un_number_vars_2(I1,F,Tail,Y).
un_number_vars_2(_1,_2,[],_3).


check_if_cge((X,Y),Result) :-
    check_if_cge(X,ResultX),
    'check_if_cge/2/1/$disj/1'(Result,Y,ResultX).
check_if_cge('=>'(_1,_2),0).
check_if_cge(&(_1,_2),0).
check_if_cge(_1,1).

'check_if_cge/2/1/$disj/1'(Result,Y,ResultX) :-
    term_basic:(ResultX=0),
    term_basic:(Result=0).
'check_if_cge/2/1/$disj/1'(Result,Y,ResultX) :-
    check_if_cge(Y,ResultY),
    arithmetic:(Result is ResultX*ResultY).

builtin(fail/0).
builtin(false/0).
builtin(otherwise/0).
builtin(repeat/0).
builtin(true/0).
builtin(version/0).
builtin(version/1).
builtin(!/0).
builtin(abolish/1).
builtin(abolish/2).
builtin(abort/0).
builtin(assert/1).
builtin(assert/2).
builtin(asserta/1).
builtin(asserta/2).
builtin(assertz/1).
builtin(assertz/2).
builtin(bagof/3).
builtin(break/0).
builtin(call/1).
builtin(close/1).
builtin(compile/1).
builtin(consult/1).
builtin(debug/0).
builtin(debugging/0).
builtin(ensure_loaded/1).
builtin(erase/1).
builtin(fileerrors/0).
builtin(flush_output/1).
builtin(foreign/2).
builtin(foreign/3).
builtin(foreign_file/2).
builtin(get/1).
builtin(get/2).
builtin(get0/1).
builtin(get0/2).
builtin(halt/0).
builtin(incore/1).
builtin(load_foreign_files/2).
builtin(module/1).
builtin(nofileerrors/0).
builtin(no_style_check/1).
builtin(open/3).
builtin(open_null_stream/1).
builtin(read/1).
builtin(read/2).
builtin(recorda/3).
builtin(recorded/3).
builtin(recordz/3).
builtin(reinitialise/0).
builtin(restore/1).
builtin(restore/2).
builtin(retract/1).
builtin(retractall/1).
builtin(save/1).
builtin(save/2).
builtin(save_program/1).
builtin(see/1).
builtin(seeing/1).
builtin(seen/0).
builtin(set_input/1).
builtin(set_output/1).
builtin(setof/3).
builtin(skip/1).
builtin(skip/2).
builtin(style_check/1).
builtin(ttyget/1).
builtin(ttyget0/1).
builtin(ttyskip/1).
builtin(unknown/2).
builtin(use_module/1).
builtin(use_module/2).
builtin(^ /2).
builtin((\+)/1).

