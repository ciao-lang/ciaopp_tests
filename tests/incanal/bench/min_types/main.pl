:- module(_,[],[]).

:- use_module(lib).

:- export(test_sorted/1).
test_sorted(L):-
    qsort(L,Result).

qsort([X|L],R) :-
    qsort(L2,R2), qsort(L1,R1), append(R1,[X|R2],R).
qsort([],[]).

