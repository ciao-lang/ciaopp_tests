:- module(find_all_vars,[find_all_vars/2],[]).

:- use_module(library(sort), [sort/2]).
:- use_module(find_all_vars2, [find_all_vars2/2]).

find_all_vars(E,Vars) :-
    find_all_vars2(E,Vars0),
    sort:sort(Vars0,Vars).
