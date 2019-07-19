:- module(intersect,[intersect/5],[]).

intersect(As,[],[],[],As) :- !.
intersect([],Bs,[],Bs,[]) :- !.
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        term_basic:(A=B),
        !,
        term_basic:(Cs=[A|Cs2]),
        intersect(As,Bs,Cs2,Ds,Es).
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        term_compare:(A@<B),
        !,
        term_basic:(Es=[A|Es2]),
        intersect(As,[B|Bs],Cs,Ds,Es2).
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        term_basic:(Ds=[B|Ds2]),
        intersect([A|As],Bs,Cs,Ds2,Es).
