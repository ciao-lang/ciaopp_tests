:- module( _aiakl, [goal/2], [assertions,nativeprops] ).

:- use_module(library(assertions/native_props), [indep/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(intersect, [intersect/5]).
:- use_module(find_all_vars, [find_all_vars/2]).

goal(E1,E2) :-
    init_vars([[a]=[any]],[[b]=[any]],E1,E2).

% :- entry init_vars(E1,E2,E1in,E2in) 
%          : ( term_typing:ground([E1,E2]), native_props:indep(E1in,E2in), term_typing:var(E1in), term_typing:var(E2in) ). 

init_vars(E1,E2,E1init,E2init) :-
    find_all_vars(E1,Vars1),
    find_all_vars(E2,Vars2),
    intersect(Vars1,Vars2,_X,Notin1,Notin2),
    init_vars2(Notin1,E1,E1init),
    init_vars2(Notin2,E2,E2init).


init_vars2(Notin,E,Einit) :-
    init_vars3(Notin,E,Einit0),
    sort:sort(Einit0,Einit).

init_vars3([],E,E).
init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
    init_vars3(Vars,E,Es).




