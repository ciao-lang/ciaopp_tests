:- module(qsort, [qsort/2],[]).

%:- use_package(assertions).

:- use_module(mylists, [append/3]).
%:- pred qsort(A,B) : list(A).

qsort([X|L],R) :-
    partition(L,X,L1,L2),
    qsort(L2,R2), qsort(L1,R1), append(R1,[X|R2],R).
qsort([],[]).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right) :-
    E < C, !,partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]) :-
    E >= C, partition(R,C,Left,Right1).

