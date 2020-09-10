:- module(find_all_vars2,[find_all_vars2/2],[]).

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
    append(Vars,AllVars1,AllVars),
    find_all_vars2(Es,AllVars1).

append([],A,A).
append([A|B],C,[A|D]) :-
    append(B,C,D).
