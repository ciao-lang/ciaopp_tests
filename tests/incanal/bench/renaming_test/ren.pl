:- module(_,[p/0],[]).

p :-
    test_ren(Rds, L3,L2),
    test_ren(Ds, Old,L2).

test_ren([D|Ds], [D|R], L2):-
    test_ren(Ds, R, L2).
test_ren([D|Ds], [X|R], [X|L2]).

