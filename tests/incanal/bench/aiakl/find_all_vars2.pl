:- module(find_all_vars2,[find_all_vars2/2],[]).

:- use_module(find_all_vars, [append/3]).

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
        append(Vars,AllVars1,AllVars),
        find_all_vars2(Es,AllVars1).

